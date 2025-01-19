#! /usr/bin/env python

#                                                                          #
#                         GNAT COMPILER COMPONENTS                         #
#                                                                          #
#                               X S P A C K                                #
#                                                                          #
#           Copyright (C) 2001-2024, Free Software Foundation, Inc.        #
#                                                                          #
# GNAT is free software;  you can  redistribute it  and/or modify it under #
# terms of the  GNU General Public License as published  by the Free Soft- #
# ware  Foundation;  either version 3,  or (at your option) any later ver- #
# sion.  GNAT is distributed in the hope that it will be useful, but WITH- #
# OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY #
# or FITNESS FOR A PARTICULAR PURPOSE.                                     #
#                                                                          #
# As a special exception under Section 7 of GPL version 3, you are granted #
# additional permissions described in the GCC Runtime Library Exception,   #
# version 3.1, as published by the Free Software Foundation.               #
#                                                                          #
# You should have received a copy of the GNU General Public License and    #
# a copy of the GCC Runtime Library Exception along with this program;     #
# see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    #
# <http://www.gnu.org/licenses/>.                                          #
#                                                                          #
# GNAT was originally developed  by the GNAT team at  New York University. #
# Extensive contributions were provided by Ada Core Technologies Inc.      #

# xspack

# Generate s-pack*.{ads,adb} from templates s-pack.ads.tmpl and s-pack.adb.tmpl

tmpl_base = "s-pack.ad%c.tmpl"


def read_template(part):
    return open(tmpl_base % part).readlines()


def output(pkg, bits, part):
    global tmpl

    bits_str_pad = "%02d" % bits
    if bits > 99:
        bits_str_dbl = "1 %d %d" % ((bits / 10) % 10, bits % 10)
    else:
        bits_str_dbl = "%d %d  " % (bits / 10, bits % 10)
    bits_str = "%d" % bits

    out = open(pkg % (bits, part), 'w')
    skip = False

    for line in tmpl[part]:
        if line.find('@even') == 0 and bits % 2 == 1:
            skip = True
        if line.find('@/even') == 0:
            skip = False
        if line[0] != '@' and not skip:
            line = line.replace('@@', bits_str_pad)
            line = line.replace('@ @  ', bits_str_dbl)
            line = line.replace('@', bits_str)
            out.write(line)


parts = ['s', 'b']
tmpl = {}

for part in parts:
    tmpl[part] = read_template(part)

for bits in range(1, 128):
    if bits & (bits - 1) == 0:
        # Power of two: no package generated
        continue

    if bits > 99:
        pkg_base = "s-pack%3d.ad%c"
    else:
        pkg_base = "s-pack%02d.ad%c"

    for part in parts:
        output(pkg_base, bits, part)
