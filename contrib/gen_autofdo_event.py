#!/usr/bin/python
# Generate Intel taken branches Linux perf event script for autofdo profiling.

# Copyright (C) 2016 Free Software Foundation, Inc.
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
# <http://www.gnu.org/licenses/>.  */

# Run it with perf record -b -e EVENT program ...
# The Linux Kernel needs to support the PMU of the current CPU, and
# It will likely not work in VMs.
# Add --all to print for all cpus, otherwise for current cpu.
# Add --script to generate shell script to run correct event.
#
# Requires internet (https) access. This may require setting up a proxy
# with export https_proxy=...
#
import urllib2
import sys
import json
import argparse
import collections

baseurl = "https://download.01.org/perfmon"

target_events = (u'BR_INST_RETIRED.NEAR_TAKEN',
                 u'BR_INST_EXEC.TAKEN',
                 u'BR_INST_RETIRED.TAKEN_JCC',
                 u'BR_INST_TYPE_RETIRED.COND_TAKEN')

ap = argparse.ArgumentParser()
ap.add_argument('--all', '-a', help='Print for all CPUs', action='store_true')
ap.add_argument('--script', help='Generate shell script', action='store_true')
args = ap.parse_args()

eventmap = collections.defaultdict(list)

def get_cpu_str():
    with open('/proc/cpuinfo', 'r') as c:
        vendor, fam, model = None, None, None
        for j in c:
            n = j.split()
            if n[0] == 'vendor_id':
                vendor = n[2]
            elif n[0] == 'model' and n[1] == ':':
                model = int(n[2])
            elif n[0] == 'cpu' and n[1] == 'family':
                fam = int(n[3])
            if vendor and fam and model:
                return "%s-%d-%X" % (vendor, fam, model), model
    return None, None

def find_event(eventurl, model):
    print >>sys.stderr, "Downloading", eventurl
    u = urllib2.urlopen(eventurl)
    events = json.loads(u.read())
    u.close()

    found = 0
    for j in events:
        if j[u'EventName'] in target_events:
            event = "cpu/event=%s,umask=%s/" % (j[u'EventCode'], j[u'UMask'])
            if u'PEBS' in j and j[u'PEBS'] > 0:
                event += "p"
            if args.script:
                eventmap[event].append(model)
            else:
                print j[u'EventName'], "event for model", model, "is", event
            found += 1
    return found

if not args.all:
    cpu, model = get_cpu_str()
    if not cpu:
        sys.exit("Unknown CPU type")

url = baseurl + "/mapfile.csv"
print >>sys.stderr, "Downloading", url
u = urllib2.urlopen(url)
found = 0
cpufound = 0
for j in u:
    n = j.rstrip().split(',')
    if len(n) >= 4 and (args.all or n[0] == cpu) and n[3] == "core":
        if args.all:
            vendor, fam, model = n[0].split("-")
            model = int(model, 16)
        cpufound += 1
        found += find_event(baseurl + n[2], model)
u.close()

if args.script:
    print '''#!/bin/sh
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

case `egrep -q "^cpu family\s*: 6" /proc/cpuinfo &&
  egrep "^model\s*:" /proc/cpuinfo | head -n1` in'''
    for event, mod in eventmap.iteritems():
        for m in mod[:-1]:
            print "model*:\ %s|\\" % m
        print 'model*:\ %s) E="%s$FLAGS" ;;' % (mod[-1], event)
    print '''*)
echo >&2 "Unknown CPU. Run contrib/gen_autofdo_event.py --all --script to update script."
	exit 1 ;;'''
    print "esac"
    print 'exec perf record -e $E -b "$@"'

if cpufound == 0 and not args.all:
    sys.exit('CPU %s not found' % cpu)

if found == 0:
    sys.exit('Branch event not found')
