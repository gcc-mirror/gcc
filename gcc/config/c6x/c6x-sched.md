;; -*- buffer-read-only: t -*-
;; Generated automatically from c6x-sched.md.in by gensched.sh

;; Definitions for side 1, cross n

;; Scheduling description for TI C6X.
;; Copyright (C) 2010, 2011 Free Software Foundation, Inc.
;; Contributed by Bernd Schmidt <bernds@codesourcery.com>
;; Contributed by CodeSourcery.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Input file for gensched.sh We process this file multiple times,
;; replacing 1 with either 1 or 2 for each of the sides of the
;; machine, and a correspondingly with "a" or "b".  n and
;;  are replaced with yes/no and the appropriate reservation.

(define_insn_reservation "load_d1n" 5
  (and (eq_attr "type" "load")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "d_addr")
		 (eq_attr "addr_regfile" "a"))))
  "d1+t1")

(define_insn_reservation "store_d1n" 1
  (and (eq_attr "type" "store")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "d_addr")
		 (eq_attr "addr_regfile" "a"))))
  "d1+t1")

(define_insn_reservation "loadn_d1n" 5
  (and (eq_attr "type" "loadn")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "d_addr")
		 (eq_attr "addr_regfile" "a"))))
  "d1+t1+t2")

(define_insn_reservation "storen_d1n" 1
  (and (eq_attr "type" "storen")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "d_addr")
		 (eq_attr "addr_regfile" "a"))))
  "d1+t1+t2")

(define_insn_reservation "single_d1n" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "d")
		 (eq_attr "dest_regfile" "a"))))
  "d1")

(define_insn_reservation "single_l1n" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "l")
		 (eq_attr "dest_regfile" "a"))))
  "l1+l1w")

(define_insn_reservation "fp4_l1n" 4
  (and (eq_attr "type" "fp4")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "l")
		 (eq_attr "dest_regfile" "a"))))
  "l1,nothing*2,l1w")

(define_insn_reservation "intdp_l1n" 5
  (and (eq_attr "type" "intdp")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "l")
		 (eq_attr "dest_regfile" "a"))))
  "l1,nothing*2,l1w*2")

(define_insn_reservation "adddp_l1n" 7
  (and (eq_attr "type" "adddp")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "l")
		 (eq_attr "dest_regfile" "a"))))
  "(l1)*2,nothing*3,l1w*2")

(define_insn_reservation "branch_s1n" 6
  (and (eq_attr "type" "branch")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "a"))))
  "(s1+s1w)+br1")

(define_insn_reservation "call_addkpc_s1n" 6
  (and (eq_attr "type" "call")
       (and (ne (symbol_ref "TARGET_INSNS_64") (const_int 0))
	    (and (eq_attr "cross" "n")
		 (and (eq_attr "units" "s")
		      (eq_attr "dest_regfile" "a")))))
  "(s1+s1w)+br1,s2+br0+br1")

(define_insn_reservation "call_mvk_s1n" 6
  (and (eq_attr "type" "call")
       (and (eq (symbol_ref "TARGET_INSNS_64") (const_int 0))
	    (and (eq_attr "cross" "n")
		 (and (eq_attr "units" "s")
		      (eq_attr "dest_regfile" "a")))))
  "(s1+s1w)+br1,s2,s2")

(define_insn_reservation "single_s1n" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "a"))))
  "(s1+s1w)")

(define_insn_reservation "cmpdp_s1n" 2
  (and (eq_attr "type" "cmpdp")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "a"))))
  "s1,(s1)+s1w")

(define_insn_reservation "dp2_s1n" 2
  (and (eq_attr "type" "dp2")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "a"))))
  "s1+s1w,s1w")

(define_insn_reservation "fp4_s1n" 4
  (and (eq_attr "type" "fp4")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "a"))))
  "s1,nothing*2,s1w")

(define_insn_reservation "mvilc4_s1n" 4
  (and (eq_attr "type" "mvilc")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "a"))))
  "(s1+s1w)")

(define_insn_reservation "single_dl1n" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "dl")
		 (eq_attr "dest_regfile" "a"))))
  "(d1|(l1+l1w))")

(define_insn_reservation "single_ds1n" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "ds")
		 (eq_attr "dest_regfile" "a"))))
  "(d1|(s1+s1w))")

(define_insn_reservation "single_ls1n" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "ls")
		 (eq_attr "dest_regfile" "a"))))
  "((l1+l1w)|(s1+s1w))")

(define_insn_reservation "dp2_l1n" 2
  (and (eq_attr "type" "dp2")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "l")
		 (eq_attr "dest_regfile" "a"))))
  "l1+l1w,l1w")

(define_insn_reservation "fp4_ls1n" 4
  (and (eq_attr "type" "fp4")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "ls")
		 (eq_attr "dest_regfile" "a"))))
  "(fps1+s1,nothing*2,s1w)|(fpl1+l1,nothing*2,l1w)")

(define_insn_reservation "adddp_ls1n" 7
  (and (eq_attr "type" "adddp")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "ls")
		 (eq_attr "dest_regfile" "a"))))
  "(adddps1+(s1)*2,nothing*3,s1w*2)|(adddpl1+(l1)*2,nothing*3,l1w*2)")

(define_insn_reservation "single_dls1n" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "dls")
		 (eq_attr "dest_regfile" "a"))))
  "(d1|(l1+l1w)|(s1+s1w))")

(define_insn_reservation "mpy2_m1n" 2
  (and (eq_attr "type" "mpy2")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "m")
		 (eq_attr "dest_regfile" "a"))))
  "m1,m1w")

(define_insn_reservation "mpy4_m1n" 4
  (and (eq_attr "type" "mpy4")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "m")
		 (eq_attr "dest_regfile" "a"))))
  "m1,nothing,nothing,m1w")

(define_insn_reservation "mpydp_m1n" 10
  (and (eq_attr "type" "mpydp")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "m")
		 (eq_attr "dest_regfile" "a"))))
  "(m1)*4,nothing*4,m1w*2")

(define_insn_reservation "mpyspdp_m1n" 7
  (and (eq_attr "type" "mpyspdp")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "m")
		 (eq_attr "dest_regfile" "a"))))
  "(m1)*2,nothing*3,m1w*2")

(define_insn_reservation "mpysp2dp_m1n" 5
  (and (eq_attr "type" "mpysp2dp")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "m")
		 (eq_attr "dest_regfile" "a"))))
  "m1,nothing*2,m1w*2")

;; Definitions for side 2, cross n

;; Scheduling description for TI C6X.
;; Copyright (C) 2010, 2011 Free Software Foundation, Inc.
;; Contributed by Bernd Schmidt <bernds@codesourcery.com>
;; Contributed by CodeSourcery.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Input file for gensched.sh We process this file multiple times,
;; replacing 2 with either 1 or 2 for each of the sides of the
;; machine, and b correspondingly with "a" or "b".  n and
;;  are replaced with yes/no and the appropriate reservation.

(define_insn_reservation "load_d2n" 5
  (and (eq_attr "type" "load")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "d_addr")
		 (eq_attr "addr_regfile" "b"))))
  "d2+t2")

(define_insn_reservation "store_d2n" 1
  (and (eq_attr "type" "store")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "d_addr")
		 (eq_attr "addr_regfile" "b"))))
  "d2+t2")

(define_insn_reservation "loadn_d2n" 5
  (and (eq_attr "type" "loadn")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "d_addr")
		 (eq_attr "addr_regfile" "b"))))
  "d2+t1+t2")

(define_insn_reservation "storen_d2n" 1
  (and (eq_attr "type" "storen")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "d_addr")
		 (eq_attr "addr_regfile" "b"))))
  "d2+t1+t2")

(define_insn_reservation "single_d2n" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "d")
		 (eq_attr "dest_regfile" "b"))))
  "d2")

(define_insn_reservation "single_l2n" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "l")
		 (eq_attr "dest_regfile" "b"))))
  "l2+l2w")

(define_insn_reservation "fp4_l2n" 4
  (and (eq_attr "type" "fp4")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "l")
		 (eq_attr "dest_regfile" "b"))))
  "l2,nothing*2,l2w")

(define_insn_reservation "intdp_l2n" 5
  (and (eq_attr "type" "intdp")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "l")
		 (eq_attr "dest_regfile" "b"))))
  "l2,nothing*2,l2w*2")

(define_insn_reservation "adddp_l2n" 7
  (and (eq_attr "type" "adddp")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "l")
		 (eq_attr "dest_regfile" "b"))))
  "(l2)*2,nothing*3,l2w*2")

(define_insn_reservation "branch_s2n" 6
  (and (eq_attr "type" "branch")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "b"))))
  "(s2+s2w)+br1")

(define_insn_reservation "call_addkpc_s2n" 6
  (and (eq_attr "type" "call")
       (and (ne (symbol_ref "TARGET_INSNS_64") (const_int 0))
	    (and (eq_attr "cross" "n")
		 (and (eq_attr "units" "s")
		      (eq_attr "dest_regfile" "b")))))
  "(s2+s2w)+br1,s2+br0+br1")

(define_insn_reservation "call_mvk_s2n" 6
  (and (eq_attr "type" "call")
       (and (eq (symbol_ref "TARGET_INSNS_64") (const_int 0))
	    (and (eq_attr "cross" "n")
		 (and (eq_attr "units" "s")
		      (eq_attr "dest_regfile" "b")))))
  "(s2+s2w)+br1,s2,s2")

(define_insn_reservation "single_s2n" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "b"))))
  "(s2+s2w)")

(define_insn_reservation "cmpdp_s2n" 2
  (and (eq_attr "type" "cmpdp")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "b"))))
  "s2,(s2)+s2w")

(define_insn_reservation "dp2_s2n" 2
  (and (eq_attr "type" "dp2")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "b"))))
  "s2+s2w,s2w")

(define_insn_reservation "fp4_s2n" 4
  (and (eq_attr "type" "fp4")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "b"))))
  "s2,nothing*2,s2w")

(define_insn_reservation "mvilc4_s2n" 4
  (and (eq_attr "type" "mvilc")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "b"))))
  "(s2+s2w)")

(define_insn_reservation "single_dl2n" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "dl")
		 (eq_attr "dest_regfile" "b"))))
  "(d2|(l2+l2w))")

(define_insn_reservation "single_ds2n" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "ds")
		 (eq_attr "dest_regfile" "b"))))
  "(d2|(s2+s2w))")

(define_insn_reservation "single_ls2n" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "ls")
		 (eq_attr "dest_regfile" "b"))))
  "((l2+l2w)|(s2+s2w))")

(define_insn_reservation "dp2_l2n" 2
  (and (eq_attr "type" "dp2")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "l")
		 (eq_attr "dest_regfile" "b"))))
  "l2+l2w,l2w")

(define_insn_reservation "fp4_ls2n" 4
  (and (eq_attr "type" "fp4")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "ls")
		 (eq_attr "dest_regfile" "b"))))
  "(fps2+s2,nothing*2,s2w)|(fpl2+l2,nothing*2,l2w)")

(define_insn_reservation "adddp_ls2n" 7
  (and (eq_attr "type" "adddp")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "ls")
		 (eq_attr "dest_regfile" "b"))))
  "(adddps2+(s2)*2,nothing*3,s2w*2)|(adddpl2+(l2)*2,nothing*3,l2w*2)")

(define_insn_reservation "single_dls2n" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "dls")
		 (eq_attr "dest_regfile" "b"))))
  "(d2|(l2+l2w)|(s2+s2w))")

(define_insn_reservation "mpy2_m2n" 2
  (and (eq_attr "type" "mpy2")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "m")
		 (eq_attr "dest_regfile" "b"))))
  "m2,m2w")

(define_insn_reservation "mpy4_m2n" 4
  (and (eq_attr "type" "mpy4")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "m")
		 (eq_attr "dest_regfile" "b"))))
  "m2,nothing,nothing,m2w")

(define_insn_reservation "mpydp_m2n" 10
  (and (eq_attr "type" "mpydp")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "m")
		 (eq_attr "dest_regfile" "b"))))
  "(m2)*4,nothing*4,m2w*2")

(define_insn_reservation "mpyspdp_m2n" 7
  (and (eq_attr "type" "mpyspdp")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "m")
		 (eq_attr "dest_regfile" "b"))))
  "(m2)*2,nothing*3,m2w*2")

(define_insn_reservation "mpysp2dp_m2n" 5
  (and (eq_attr "type" "mpysp2dp")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "m")
		 (eq_attr "dest_regfile" "b"))))
  "m2,nothing*2,m2w*2")

;; Definitions for side 1, cross y

;; Scheduling description for TI C6X.
;; Copyright (C) 2010, 2011 Free Software Foundation, Inc.
;; Contributed by Bernd Schmidt <bernds@codesourcery.com>
;; Contributed by CodeSourcery.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Input file for gensched.sh We process this file multiple times,
;; replacing 1 with either 1 or 2 for each of the sides of the
;; machine, and a correspondingly with "a" or "b".  y and
;; +x1 are replaced with yes/no and the appropriate reservation.

(define_insn_reservation "load_d1y" 5
  (and (eq_attr "type" "load")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "d_addr")
		 (eq_attr "addr_regfile" "a"))))
  "d1+t2")

(define_insn_reservation "store_d1y" 1
  (and (eq_attr "type" "store")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "d_addr")
		 (eq_attr "addr_regfile" "a"))))
  "d1+t2")

(define_insn_reservation "loadn_d1y" 5
  (and (eq_attr "type" "loadn")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "d_addr")
		 (eq_attr "addr_regfile" "a"))))
  "d1+t1+t2")

(define_insn_reservation "storen_d1y" 1
  (and (eq_attr "type" "storen")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "d_addr")
		 (eq_attr "addr_regfile" "a"))))
  "d1+t1+t2")

(define_insn_reservation "single_d1y" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "d")
		 (eq_attr "dest_regfile" "a"))))
  "d1+x1")

(define_insn_reservation "single_l1y" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "l")
		 (eq_attr "dest_regfile" "a"))))
  "l1+l1w+x1")

(define_insn_reservation "fp4_l1y" 4
  (and (eq_attr "type" "fp4")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "l")
		 (eq_attr "dest_regfile" "a"))))
  "l1+x1,nothing*2,l1w")

(define_insn_reservation "intdp_l1y" 5
  (and (eq_attr "type" "intdp")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "l")
		 (eq_attr "dest_regfile" "a"))))
  "l1+x1,nothing*2,l1w*2")

(define_insn_reservation "adddp_l1y" 7
  (and (eq_attr "type" "adddp")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "l")
		 (eq_attr "dest_regfile" "a"))))
  "(l1+x1)*2,nothing*3,l1w*2")

(define_insn_reservation "branch_s1y" 6
  (and (eq_attr "type" "branch")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "a"))))
  "(s1+s1w)+x1+br1")

(define_insn_reservation "call_addkpc_s1y" 6
  (and (eq_attr "type" "call")
       (and (ne (symbol_ref "TARGET_INSNS_64") (const_int 0))
	    (and (eq_attr "cross" "y")
		 (and (eq_attr "units" "s")
		      (eq_attr "dest_regfile" "a")))))
  "(s1+s1w)+x1+br1,s2+br0+br1")

(define_insn_reservation "call_mvk_s1y" 6
  (and (eq_attr "type" "call")
       (and (eq (symbol_ref "TARGET_INSNS_64") (const_int 0))
	    (and (eq_attr "cross" "y")
		 (and (eq_attr "units" "s")
		      (eq_attr "dest_regfile" "a")))))
  "(s1+s1w)+x1+br1,s2,s2")

(define_insn_reservation "single_s1y" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "a"))))
  "(s1+s1w)+x1")

(define_insn_reservation "cmpdp_s1y" 2
  (and (eq_attr "type" "cmpdp")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "a"))))
  "s1+x1,(s1+x1)+s1w")

(define_insn_reservation "dp2_s1y" 2
  (and (eq_attr "type" "dp2")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "a"))))
  "s1+s1w+x1,s1w")

(define_insn_reservation "fp4_s1y" 4
  (and (eq_attr "type" "fp4")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "a"))))
  "s1+x1,nothing*2,s1w")

(define_insn_reservation "mvilc4_s1y" 4
  (and (eq_attr "type" "mvilc")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "a"))))
  "(s1+s1w)+x1")

(define_insn_reservation "single_dl1y" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "dl")
		 (eq_attr "dest_regfile" "a"))))
  "(d1|(l1+l1w))+x1")

(define_insn_reservation "single_ds1y" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "ds")
		 (eq_attr "dest_regfile" "a"))))
  "(d1|(s1+s1w))+x1")

(define_insn_reservation "single_ls1y" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "ls")
		 (eq_attr "dest_regfile" "a"))))
  "((l1+l1w)|(s1+s1w))+x1")

(define_insn_reservation "dp2_l1y" 2
  (and (eq_attr "type" "dp2")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "l")
		 (eq_attr "dest_regfile" "a"))))
  "l1+l1w+x1,l1w")

(define_insn_reservation "fp4_ls1y" 4
  (and (eq_attr "type" "fp4")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "ls")
		 (eq_attr "dest_regfile" "a"))))
  "(fps1+s1+x1,nothing*2,s1w)|(fpl1+l1+x1,nothing*2,l1w)")

(define_insn_reservation "adddp_ls1y" 7
  (and (eq_attr "type" "adddp")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "ls")
		 (eq_attr "dest_regfile" "a"))))
  "(adddps1+(s1+x1)*2,nothing*3,s1w*2)|(adddpl1+(l1+x1)*2,nothing*3,l1w*2)")

(define_insn_reservation "single_dls1y" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "dls")
		 (eq_attr "dest_regfile" "a"))))
  "(d1|(l1+l1w)|(s1+s1w))+x1")

(define_insn_reservation "mpy2_m1y" 2
  (and (eq_attr "type" "mpy2")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "m")
		 (eq_attr "dest_regfile" "a"))))
  "m1+x1,m1w")

(define_insn_reservation "mpy4_m1y" 4
  (and (eq_attr "type" "mpy4")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "m")
		 (eq_attr "dest_regfile" "a"))))
  "m1+x1,nothing,nothing,m1w")

(define_insn_reservation "mpydp_m1y" 10
  (and (eq_attr "type" "mpydp")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "m")
		 (eq_attr "dest_regfile" "a"))))
  "(m1+x1)*4,nothing*4,m1w*2")

(define_insn_reservation "mpyspdp_m1y" 7
  (and (eq_attr "type" "mpyspdp")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "m")
		 (eq_attr "dest_regfile" "a"))))
  "(m1+x1)*2,nothing*3,m1w*2")

(define_insn_reservation "mpysp2dp_m1y" 5
  (and (eq_attr "type" "mpysp2dp")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "m")
		 (eq_attr "dest_regfile" "a"))))
  "m1+x1,nothing*2,m1w*2")

;; Definitions for side 2, cross y

;; Scheduling description for TI C6X.
;; Copyright (C) 2010, 2011 Free Software Foundation, Inc.
;; Contributed by Bernd Schmidt <bernds@codesourcery.com>
;; Contributed by CodeSourcery.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Input file for gensched.sh We process this file multiple times,
;; replacing 2 with either 1 or 2 for each of the sides of the
;; machine, and b correspondingly with "a" or "b".  y and
;; +x2 are replaced with yes/no and the appropriate reservation.

(define_insn_reservation "load_d2y" 5
  (and (eq_attr "type" "load")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "d_addr")
		 (eq_attr "addr_regfile" "b"))))
  "d2+t1")

(define_insn_reservation "store_d2y" 1
  (and (eq_attr "type" "store")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "d_addr")
		 (eq_attr "addr_regfile" "b"))))
  "d2+t1")

(define_insn_reservation "loadn_d2y" 5
  (and (eq_attr "type" "loadn")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "d_addr")
		 (eq_attr "addr_regfile" "b"))))
  "d2+t1+t2")

(define_insn_reservation "storen_d2y" 1
  (and (eq_attr "type" "storen")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "d_addr")
		 (eq_attr "addr_regfile" "b"))))
  "d2+t1+t2")

(define_insn_reservation "single_d2y" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "d")
		 (eq_attr "dest_regfile" "b"))))
  "d2+x2")

(define_insn_reservation "single_l2y" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "l")
		 (eq_attr "dest_regfile" "b"))))
  "l2+l2w+x2")

(define_insn_reservation "fp4_l2y" 4
  (and (eq_attr "type" "fp4")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "l")
		 (eq_attr "dest_regfile" "b"))))
  "l2+x2,nothing*2,l2w")

(define_insn_reservation "intdp_l2y" 5
  (and (eq_attr "type" "intdp")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "l")
		 (eq_attr "dest_regfile" "b"))))
  "l2+x2,nothing*2,l2w*2")

(define_insn_reservation "adddp_l2y" 7
  (and (eq_attr "type" "adddp")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "l")
		 (eq_attr "dest_regfile" "b"))))
  "(l2+x2)*2,nothing*3,l2w*2")

(define_insn_reservation "branch_s2y" 6
  (and (eq_attr "type" "branch")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "b"))))
  "(s2+s2w)+x2+br1")

(define_insn_reservation "call_addkpc_s2y" 6
  (and (eq_attr "type" "call")
       (and (ne (symbol_ref "TARGET_INSNS_64") (const_int 0))
	    (and (eq_attr "cross" "y")
		 (and (eq_attr "units" "s")
		      (eq_attr "dest_regfile" "b")))))
  "(s2+s2w)+x2+br1,s2+br0+br1")

(define_insn_reservation "call_mvk_s2y" 6
  (and (eq_attr "type" "call")
       (and (eq (symbol_ref "TARGET_INSNS_64") (const_int 0))
	    (and (eq_attr "cross" "y")
		 (and (eq_attr "units" "s")
		      (eq_attr "dest_regfile" "b")))))
  "(s2+s2w)+x2+br1,s2,s2")

(define_insn_reservation "single_s2y" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "b"))))
  "(s2+s2w)+x2")

(define_insn_reservation "cmpdp_s2y" 2
  (and (eq_attr "type" "cmpdp")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "b"))))
  "s2+x2,(s2+x2)+s2w")

(define_insn_reservation "dp2_s2y" 2
  (and (eq_attr "type" "dp2")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "b"))))
  "s2+s2w+x2,s2w")

(define_insn_reservation "fp4_s2y" 4
  (and (eq_attr "type" "fp4")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "b"))))
  "s2+x2,nothing*2,s2w")

(define_insn_reservation "mvilc4_s2y" 4
  (and (eq_attr "type" "mvilc")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "b"))))
  "(s2+s2w)+x2")

(define_insn_reservation "single_dl2y" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "dl")
		 (eq_attr "dest_regfile" "b"))))
  "(d2|(l2+l2w))+x2")

(define_insn_reservation "single_ds2y" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "ds")
		 (eq_attr "dest_regfile" "b"))))
  "(d2|(s2+s2w))+x2")

(define_insn_reservation "single_ls2y" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "ls")
		 (eq_attr "dest_regfile" "b"))))
  "((l2+l2w)|(s2+s2w))+x2")

(define_insn_reservation "dp2_l2y" 2
  (and (eq_attr "type" "dp2")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "l")
		 (eq_attr "dest_regfile" "b"))))
  "l2+l2w+x2,l2w")

(define_insn_reservation "fp4_ls2y" 4
  (and (eq_attr "type" "fp4")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "ls")
		 (eq_attr "dest_regfile" "b"))))
  "(fps2+s2+x2,nothing*2,s2w)|(fpl2+l2+x2,nothing*2,l2w)")

(define_insn_reservation "adddp_ls2y" 7
  (and (eq_attr "type" "adddp")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "ls")
		 (eq_attr "dest_regfile" "b"))))
  "(adddps2+(s2+x2)*2,nothing*3,s2w*2)|(adddpl2+(l2+x2)*2,nothing*3,l2w*2)")

(define_insn_reservation "single_dls2y" 1
  (and (eq_attr "type" "single")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "dls")
		 (eq_attr "dest_regfile" "b"))))
  "(d2|(l2+l2w)|(s2+s2w))+x2")

(define_insn_reservation "mpy2_m2y" 2
  (and (eq_attr "type" "mpy2")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "m")
		 (eq_attr "dest_regfile" "b"))))
  "m2+x2,m2w")

(define_insn_reservation "mpy4_m2y" 4
  (and (eq_attr "type" "mpy4")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "m")
		 (eq_attr "dest_regfile" "b"))))
  "m2+x2,nothing,nothing,m2w")

(define_insn_reservation "mpydp_m2y" 10
  (and (eq_attr "type" "mpydp")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "m")
		 (eq_attr "dest_regfile" "b"))))
  "(m2+x2)*4,nothing*4,m2w*2")

(define_insn_reservation "mpyspdp_m2y" 7
  (and (eq_attr "type" "mpyspdp")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "m")
		 (eq_attr "dest_regfile" "b"))))
  "(m2+x2)*2,nothing*3,m2w*2")

(define_insn_reservation "mpysp2dp_m2y" 5
  (and (eq_attr "type" "mpysp2dp")
       (and (eq_attr "cross" "y")
	    (and (eq_attr "units" "m")
		 (eq_attr "dest_regfile" "b"))))
  "m2+x2,nothing*2,m2w*2")
