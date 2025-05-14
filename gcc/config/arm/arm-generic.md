;; Generic ARM Pipeline Description
;; Copyright (C) 2003-2025 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.  */

(define_automaton "arm")

;; Write buffer
;
; Strictly, we should model a 4-deep write buffer for ARM7xx based chips
;
; The write buffer on some of the arm6 processors is hard to model exactly.
; There is room in the buffer for up to two addresses and up to eight words
; of memory, but the two needn't be split evenly.  When writing the two
; addresses are fully pipelined.  However, a read from memory that is not
; currently in the cache will block until the writes have completed.
; It is normally the case that FCLK and MCLK will be in the ratio 2:1, so
; writes will take 2 FCLK cycles per word, if FCLK and MCLK are asynchronous
; (they aren't allowed to be at present) then there is a startup cost of 1MCLK
; cycle to add as well.
(define_cpu_unit "write_buf" "arm")

;; Write blockage unit
;
; The write_blockage unit models (partially), the fact that reads will stall
; until the write buffer empties.
(define_cpu_unit "write_blockage" "arm")

;; Core
;
(define_cpu_unit "core" "arm")

(define_insn_reservation "store_wbuf" 5
  (and (eq_attr "generic_sched" "yes")
       (and (eq_attr "model_wbuf" "yes")
       	    (eq_attr "type" "store_4")))
  "core+write_buf*3+write_blockage*5")

(define_insn_reservation "store2_wbuf" 7
  (and (eq_attr "generic_sched" "yes")
       (and (eq_attr "model_wbuf" "yes")
	    (eq_attr "type" "store_8")))
  "core+write_buf*4+write_blockage*7")

(define_insn_reservation "store3_wbuf" 9
  (and (eq_attr "generic_sched" "yes")
       (and (eq_attr "model_wbuf" "yes")
	    (eq_attr "type" "store_12")))
  "core+write_buf*5+write_blockage*9")

(define_insn_reservation "store4_wbuf" 11
  (and (eq_attr "generic_sched" "yes")
       (and (eq_attr "model_wbuf" "yes")
            (eq_attr "type" "store_16")))
  "core+write_buf*6+write_blockage*11")

(define_insn_reservation "store2" 3
  (and (eq_attr "generic_sched" "yes")
       (and (eq_attr "model_wbuf" "no")
            (eq_attr "type" "store_8")))
  "core*3")

(define_insn_reservation "store3" 4
  (and (eq_attr "generic_sched" "yes")
       (and (eq_attr "model_wbuf" "no")
            (eq_attr "type" "store_12")))
  "core*4")

(define_insn_reservation "store4" 5
  (and (eq_attr "generic_sched" "yes")
       (and (eq_attr "model_wbuf" "no")
	    (eq_attr "type" "store_16")))
  "core*5")

(define_insn_reservation "store_ldsched" 1
  (and (eq_attr "generic_sched" "yes")
       (and (eq_attr "ldsched" "yes") 
	    (eq_attr "type" "store_4")))
  "core")

(define_insn_reservation "load_ldsched_xscale" 3
  (and (eq_attr "generic_sched" "yes")
       (and (eq_attr "ldsched" "yes") 
	    (and (eq_attr "type" "load_byte,load_4")
	         (eq_attr "tune" "xscale"))))
  "core")

(define_insn_reservation "load_ldsched" 2
  (and (eq_attr "generic_sched" "yes")
       (and (eq_attr "ldsched" "yes") 
	    (and (eq_attr "type" "load_byte,load_4")
	         (eq_attr "tune" "!xscale"))))
  "core")

(define_insn_reservation "load_or_store" 2
  (and (eq_attr "generic_sched" "yes")
       (and (eq_attr "ldsched" "!yes") 
	    (eq_attr "type" "load_byte,load_4,load_8,load_12,load_16,store_4")))
  "core*2")

(define_insn_reservation "mult" 16
  (and (eq_attr "generic_sched" "yes")
       (and (eq_attr "ldsched" "no")
	    (ior (eq_attr "mul32" "yes")
		 (eq_attr "widen_mul64" "yes"))))
  "core*16")

(define_insn_reservation "mult_ldsched_strongarm" 3
  (and (eq_attr "generic_sched" "yes")
       (and (eq_attr "ldsched" "yes") 
	    (and (eq_attr "tune" "strongarm")
		 (ior (eq_attr "mul32" "yes")
		      (eq_attr "widen_mul64" "yes")))))
  "core*2")

(define_insn_reservation "mult_ldsched" 4
  (and (eq_attr "generic_sched" "yes")
       (and (eq_attr "ldsched" "yes") 
	    (and (eq_attr "tune" "!strongarm")
	         (ior (eq_attr "mul32" "yes")
		      (eq_attr "widen_mul64" "yes")))))
  "core*4")

(define_insn_reservation "multi_cycle" 32
  (and (eq_attr "generic_sched" "yes")
       (and (eq_attr "core_cycles" "multi")
            (and (eq_attr "type" "!load_byte,load_4,load_8,load_12,load_16,\
                                  store_4,store_8,store_12,store_16")
		 (not (ior (eq_attr "mul32" "yes")
			   (eq_attr "widen_mul64" "yes"))))))
  "core*32")

(define_insn_reservation "single_cycle" 1
  (and (eq_attr "generic_sched" "yes")
       (eq_attr "core_cycles" "single"))
  "core")
