;; Scheduling description for Niagara-2 and Niagara-3.
;;   Copyright (C) 2007-2017 Free Software Foundation, Inc.
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

;; Niagara-2 and Niagara-3 are single-issue processors.

(define_automaton "niagara2_0")

(define_cpu_unit "niag2_pipe" "niagara2_0")

(define_insn_reservation "niag2_25cycle" 25
  (and (eq_attr "cpu" "niagara2,niagara3")
    (eq_attr "type" "flushw"))
  "niag2_pipe*25")

(define_insn_reservation "niag2_5cycle" 5
  (and (eq_attr "cpu" "niagara2,niagara3")
    (eq_attr "type" "multi,flushw,iflush,trap"))
  "niag2_pipe*5")

(define_insn_reservation "niag2_6cycle" 4
  (and (eq_attr "cpu" "niagara2,niagara3")
    (eq_attr "type" "savew"))
  "niag2_pipe*4")

/* Most basic operations are single-cycle. */
(define_insn_reservation "niag2_ialu" 1
 (and (eq_attr "cpu" "niagara2,niagara3")
   (eq_attr "type" "ialu,shift,compare,cmove"))
 "niag2_pipe")

(define_insn_reservation "niag2_imul" 5
 (and (eq_attr "cpu" "niagara2")
   (eq_attr "type" "imul"))
 "niag2_pipe*5")

(define_insn_reservation "niag3_imul" 9
 (and (eq_attr "cpu" "niagara3")
   (eq_attr "type" "imul"))
 "niag2_pipe*9")

(define_insn_reservation "niag2_idiv" 26
 (and (eq_attr "cpu" "niagara2")
   (eq_attr "type" "idiv"))
 "niag2_pipe*26")

(define_insn_reservation "niag3_idiv" 31
 (and (eq_attr "cpu" "niagara3")
   (eq_attr "type" "idiv"))
 "niag2_pipe*31")

(define_insn_reservation "niag2_branch" 5
  (and (eq_attr "cpu" "niagara2,niagara3")
    (eq_attr "type" "call,sibcall,call_no_delay_slot,uncond_branch,branch"))
  "niag2_pipe*5")

(define_insn_reservation "niag2_3cycle_load" 3
  (and (eq_attr "cpu" "niagara2,niagara3")
    (eq_attr "type" "load,fpload"))
  "niag2_pipe*3")

(define_insn_reservation "niag2_1cycle_store" 1
  (and (eq_attr "cpu" "niagara2,niagara3")
    (eq_attr "type" "store,fpstore"))
  "niag2_pipe")

(define_insn_reservation "niag2_fp" 6
  (and (eq_attr "cpu" "niagara2")
    (eq_attr "type" "fpmove,fpcmove,fpcrmove,fpcmp,fpmul"))
  "niag2_pipe*6")

(define_insn_reservation "niag3_fp" 9
  (and (eq_attr "cpu" "niagara3")
    (eq_attr "type" "fpmove,fpcmove,fpcrmove,fpcmp,fpmul"))
  "niag2_pipe*9")

(define_insn_reservation "niag2_fdivs" 19
  (and (eq_attr "cpu" "niagara2")
    (eq_attr "type" "fpdivs"))
  "niag2_pipe*19")

(define_insn_reservation "niag3_fdivs" 23
  (and (eq_attr "cpu" "niagara3")
    (eq_attr "type" "fpdivs"))
  "niag2_pipe*23")

(define_insn_reservation "niag2_fdivd" 33
  (and (eq_attr "cpu" "niagara2")
    (eq_attr "type" "fpdivd"))
  "niag2_pipe*33")

(define_insn_reservation "niag3_fdivd" 37
  (and (eq_attr "cpu" "niagara3")
    (eq_attr "type" "fpdivd"))
  "niag2_pipe*37")

(define_insn_reservation "niag2_vis" 6
  (and (eq_attr "cpu" "niagara2")
    (eq_attr "type" "fga,vismv,visl,viscmp,fgm_pack,fgm_mul,pdist,edge,edgen,array,bmask,gsr"))
  "niag2_pipe*6")

(define_insn_reservation "niag3_vis" 9
  (and (eq_attr "cpu" "niagara3")
    (eq_attr "type" "fga,vismv,visl,viscmp,fgm_pack,fgm_mul,pdist,pdistn,edge,edgen,array,bmask,gsr"))
  "niag2_pipe*9")
