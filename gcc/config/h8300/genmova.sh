#!/bin/sh
# Generate mova.md, a file containing patterns that can be implemented
# using the h8sx mova instruction.

# Copyright (C) 2004-2013 Free Software Foundation, Inc.
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

echo ";; -*- buffer-read-only: t -*-"
echo ";; Generated automatically from genmova.sh"
echo ";; Copyright (C) 2004-2013 Free Software Foundation, Inc."
echo ";;"
echo ";; This file is part of GCC."
echo ";;"
echo ";; GCC is free software; you can redistribute it and/or modify"
echo ";; it under the terms of the GNU General Public License as published by"
echo ";; the Free Software Foundation; either version 3, or (at your option)"
echo ";; any later version."
echo ";;"
echo ";; GCC is distributed in the hope that it will be useful,"
echo ";; but WITHOUT ANY WARRANTY; without even the implied warranty of"
echo ";; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
echo ";; GNU General Public License for more details."
echo ";;"
echo ";; You should have received a copy of the GNU General Public License"
echo ";; along with GCC; see the file COPYING3.  If not see"
echo ";; <http://www.gnu.org/licenses/>."

# Loop over modes for the source operand (the index).  Only 8-bit and
# 16-bit indices are allowed.
for s in QI HI; do

  # Set $src to the operand syntax for this size of index.
  case $s in
    QI) src=%X1.b;;
    HI) src=%T1.w;;
  esac

  # A match_operand for the source.
  operand="(match_operand:$s 1 \"h8300_dst_operand\" \"0,rQ\")"

  # Loop over the destination register's mode.  The QI and HI versions use
  # the same instructions as the SI ones, they just ignore the upper bits
  # of the result.
  for d in QI HI SI; do

    # If the destination is larger than the source, include a
    # zero_extend/plus pattern.  We could also match zero extensions
    # of memory without the plus, but it's not any smaller or faster
    # than separate insns.
    case $d:$s in
      SI:QI | SI:HI | HI:QI)
	cat <<EOF
(define_insn ""
  [(set (match_operand:$d 0 "register_operand" "=r,r")
	(plus:$d (zero_extend:$d $operand)
		 (match_operand:$d 2 "immediate_operand" "i,i")))]
  "TARGET_H8300SX"
  "mova/b.l @(%o2%C2,$src),%S0"
  [(set_attr "length_table" "mova")
   (set_attr "cc" "none")])

EOF
	;;
    esac

    # Loop over the shift amount.
    for shift in 1 2; do
      case $shift in
	1) opsize=w mult=2;;
	2) opsize=l mult=4;;
      esac

      # Calculate the mask of bits that will be nonzero after the source
      # has been extended and shifted.
      case $s:$shift in
	QI:1) mask=510;;
	QI:2) mask=1020;;
	HI:1) mask=131070;;
	HI:2) mask=262140;;
      esac

      # There doesn't seem to be a well-established canonical form for
      # some of the patterns we need.  Emit both shift and multiplication
      # patterns.
      for form in mult ashift; do
	case $form in
	  mult) amount=$mult;;
	  ashift) amount=$shift;;
	esac

	case $d:$s in
	  # If the source and destination are the same size, we can treat
	  # mova as a sort of multiply-add instruction.
	  QI:QI | HI:HI)
	    cat <<EOF
(define_insn ""
  [(set (match_operand:$d 0 "register_operand" "=r,r")
	(plus:$d ($form:$d $operand
			   (const_int $amount))
		 (match_operand:$d 2 "immediate_operand" "i,i")))]
  "TARGET_H8300SX"
  "mova/$opsize.l @(%o2%C2,$src),%S0"
  [(set_attr "length_table" "mova")
   (set_attr "cc" "none")])

EOF
	    ;;

	  # Handle the cases where the source is smaller than the
	  # destination.  Sometimes combine will keep the extension,
	  # sometimes it will use an AND.
	  SI:QI | SI:HI | HI:QI)

	    # Emit the forms that use zero_extend.
	    cat <<EOF
(define_insn ""
  [(set (match_operand:$d 0 "register_operand" "=r,r")
	($form:$d (zero_extend:$d $operand)
		  (const_int $amount)))]
  "TARGET_H8300SX"
  "mova/$opsize.l @(0,$src),%S0"
  [(set_attr "length_table" "mova_zero")
   (set_attr "cc" "none")])

(define_insn ""
  [(set (match_operand:$d 0 "register_operand" "=r,r")
	(plus:$d ($form:$d (zero_extend:$d $operand)
			   (const_int $amount))
		 (match_operand:$d 2 "immediate_operand" "i,i")))]
  "TARGET_H8300SX"
  "mova/$opsize.l @(%o2%C2,$src),%S0"
  [(set_attr "length_table" "mova")
   (set_attr "cc" "none")])

EOF

	    # Now emit the forms that use AND.  When the index is a register,
	    # these forms are effectively $d-mode operations: the index will
	    # be a $d-mode REG or SUBREG.  When the index is a memory
	    # location, we will have a paradoxical subreg such as:
	    #
	    #	(and:SI (mult:SI (subreg:SI (mem:QI ...) 0)
	    #			 (const_int 4))
	    #		(const_int 1020))
	    #
	    # Match the two case separately: a $d-mode register_operand
	    # or a $d-mode subreg of an $s-mode memory_operand.  Match the
	    # memory form first since register_operand accepts mem subregs
	    # before reload.
	    memory="(match_operand:$s 1 \"memory_operand\" \"m\")"
	    memory="(subreg:$d $memory 0)"
	    register="(match_operand:$d 1 \"register_operand\" \"0\")"
	    for paradoxical in "$memory" "$register"; do
	      cat <<EOF
(define_insn ""
  [(set (match_operand:$d 0 "register_operand" "=r")
	(and:$d ($form:$d $paradoxical
			  (const_int $amount))
		(const_int $mask)))]
  "TARGET_H8300SX"
  "mova/$opsize.l @(0,$src),%S0"
  [(set_attr "length_table" "mova_zero")
   (set_attr "cc" "none")])

(define_insn ""
  [(set (match_operand:$d 0 "register_operand" "=r")
	(plus:$d (and:$d ($form:$d $paradoxical
				   (const_int $amount))
			 (const_int $mask))
		 (match_operand:$d 2 "immediate_operand" "i")))]
  "TARGET_H8300SX"
  "mova/$opsize.l @(%o2%C2,$src),%S0"
  [(set_attr "length_table" "mova")
   (set_attr "cc" "none")])

EOF
	      done
	    ;;
	esac
      done
    done
  done
done
