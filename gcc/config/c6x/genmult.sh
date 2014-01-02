#! /bin/sh
# Generate c6x-mult.md from c6x-mult.md.in
# The input file is passed as an argument.

# Copyright (C) 2011-2014 Free Software Foundation, Inc.

#This file is part of GCC.

#GCC is free software; you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation; either version 3, or (at your option)
#any later version.

#GCC is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General Public License
#along with GCC; see the file COPYING3.  If not see
#<http://www.gnu.org/licenses/>.

echo ";; -*- buffer-read-only: t -*-"
echo ";; Generated automatically from c6x-mult.md.in by genmult.sh"

sed -e "s,_VARIANT_,,g" -e "s,_SET_,set,g" -e "s,_.BRK_,,g" \
    -e "s,_A_,a,g" -e "s,_B_,b,g" -e "s,_DESTOPERAND_,register_operand,g" \
    -e "s,_MOD._,,g" -e "s,:_M,:,g" < $1

sed -e "s,_VARIANT_,_real,g" -e "s,_SET_,unspec,g" -e "s,_OBRK_,[,g" \
    -e "s,_CBRK_,] UNSPEC_REAL_MULT,g" -e "s,_A_,JA,g" -e "s,_B_,JB,g" \
    -e "s,_DESTOPERAND_,const_int_operand,g" -e "s,_MODk_,k,g" \
    -e "s,_MODK_,K,g" -e 's,:_MV..[IQ],:SI,g' -e "s,:_MSQ,:SI,g"  < $1
