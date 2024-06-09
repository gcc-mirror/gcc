#!/usr/bin/python3
# Generate Intel taken branches Linux perf event script for autofdo profiling.

# Copyright (C) 2016-2024 Free Software Foundation, Inc.
#
# GCC is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.
#
# GCC is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

# Run it with perf record -b -e EVENT program ...
# The Linux Kernel needs to support the PMU of the current CPU, and
# It will likely not work in VMs.
# Add --all to print for all cpus, otherwise for current cpu.
# Add --script to generate shell script to run correct event.
#
# Requires internet (https) access. This may require setting up a proxy
# with export https_proxy=...
#
import urllib.request
import sys
import json
import argparse
import collections
import os
import fnmatch

baseurl = "https://raw.githubusercontent.com/intel/perfmon/main"

target_events = ('BR_INST_RETIRED.NEAR_TAKEN',
                 'BR_INST_EXEC.TAKEN',
                 'BR_INST_RETIRED.TAKEN_JCC',
                 'BR_INST_TYPE_RETIRED.COND_TAKEN')

ap = argparse.ArgumentParser()
ap.add_argument('--all', '-a', help='Print for all CPUs', action='store_true')
ap.add_argument('--script', help='Generate shell script', action='store_true')
args = ap.parse_args()

eventmap = collections.defaultdict(list)

def get_cpustr():
    cpuinfo = os.getenv("CPUINFO")
    if cpuinfo is None:
        cpuinfo = '/proc/cpuinfo'
    f = open(cpuinfo, 'r')
    cpu = [None, None, None, None]
    for j in f:
        n = j.split()
        if n[0] == 'vendor_id':
            cpu[0] = n[2]
        elif n[0] == 'model' and n[1] == ':':
            cpu[2] = int(n[2])
        elif n[0] == 'cpu' and n[1] == 'family':
            cpu[1] = int(n[3])
        elif n[0] == 'stepping' and n[1] == ':':
            cpu[3] = int(n[2])
        if all(v is not None for v in cpu):
            break
    # stepping for SKX only
    stepping = cpu[0] == "GenuineIntel" and cpu[1] == 6 and cpu[2] == 0x55
    if stepping:
        return "%s-%d-%X-%X" % tuple(cpu)
    return "%s-%d-%X" % tuple(cpu)[:3]

def find_event(eventurl, model):
    print("Downloading", eventurl, file = sys.stderr)
    u = urllib.request.urlopen(eventurl)
    events = json.loads(u.read())["Events"]
    u.close()

    found = 0
    for j in events:
        if j['EventName'] in target_events:
            event = "cpu/event=%s,umask=%s/" % (j['EventCode'], j['UMask'])
            if 'PEBS' in j and int(j['PEBS']) > 0:
                event += "p"
            if args.script:
                eventmap[event].append(model)
            else:
                print(j['EventName'], "event for model", model, "is", event)
            found += 1
    return found

if not args.all:
    cpu = get_cpustr()
    if not cpu:
        sys.exit("Unknown CPU type")

url = baseurl + "/mapfile.csv"
print("Downloading", url, file = sys.stderr)
u = urllib.request.urlopen(url)
found = 0
cpufound = 0
for j in u:
    n = j.rstrip().decode().split(',')
    if len(n) >= 4 and (args.all or fnmatch.fnmatch(cpu, n[0])) and n[3] == "core":
        components = n[0].split("-")
        model = components[2]
        model = int(model, 16)
        cpufound += 1
        found += find_event(baseurl + n[2], model)
u.close()

if args.script:
    print('''#!/bin/sh
# Profile workload for gcc profile feedback (autofdo) using Linux perf.
# Auto generated. To regenerate for new CPUs run
# contrib/gen_autofdo_event.py --script --all in gcc source

# usages:
# gcc-auto-profile program             (profile program and children)
# gcc-auto-profile -a sleep X          (profile all for X secs, may need root)
# gcc-auto-profile -p PID sleep X      (profile PID)
# gcc-auto-profile --kernel -a sleep X (profile kernel)
# gcc-auto-profile --all -a sleep X    (profile kernel and user space)

# Identify branches taken event for CPU.
#

FLAGS=u

if [ "$1" = "--kernel" ] ; then
  FLAGS=k
  shift
fi
if [ "$1" = "--all" ] ; then
  FLAGS=uk
  shift
fi

if ! grep -q Intel /proc/cpuinfo ; then
  echo >&2 "Only Intel CPUs supported"
  exit 1
fi

if grep -q hypervisor /proc/cpuinfo ; then
  echo >&2 "Warning: branch profiling may not be functional in VMs"
fi

case `grep -E -q "^cpu family\s*: 6" /proc/cpuinfo &&
  grep -E "^model\s*:" /proc/cpuinfo | head -n1` in''')
    for event, mod in eventmap.items():
        for m in mod[:-1]:
            print("model*:\ %s|\\" % m)
        print('model*:\ %s) E="%s$FLAGS" ;;' % (mod[-1], event))
    print('''*)
echo >&2 "Unknown CPU. Run contrib/gen_autofdo_event.py --all --script to update script."
	exit 1 ;;''')
    print("esac")
    print("set -x")
    print('if ! perf record -e $E -b "$@" ; then')
    print('  # PEBS may not actually be working even if the processor supports it')
    print('  # (e.g., in a virtual machine). Trying to run without /p.')
    print('  set +x')
    print('  echo >&2 "Retrying without /p."')
    print('  E="$(echo "${E}" | sed -e \'s/\/p/\//\')"')
    print('  set -x')
    print('  exec perf record -e $E -b "$@"')
    print(' set +x')
    print('fi')

if cpufound == 0 and not args.all:
    sys.exit('CPU %s not found' % cpu)

if found == 0:
    sys.exit('Branch event not found')
