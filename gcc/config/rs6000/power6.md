;; Scheduling description for IBM POWER6 processor.
;;   Copyright (C) 2006-2023 Free Software Foundation, Inc.
;;   Contributed by Peter Steinmetz (steinmtz@us.ibm.com)
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Sources:

;; The POWER6 has 2 iu, 2 fpu, 2 lsu, and 1 bu/cru unit per engine 
;; (2 engines per chip).  The chip can issue up to 5 internal ops 
;; per cycle.

(define_automaton "power6iu,power6lsu,power6fpu,power6bu")

(define_cpu_unit "iu1_power6,iu2_power6" "power6iu")
(define_cpu_unit "lsu1_power6,lsu2_power6" "power6lsu")
(define_cpu_unit "bpu_power6" "power6bu")
(define_cpu_unit "fpu1_power6,fpu2_power6" "power6fpu")

(define_reservation "LS2_power6"
                    "lsu1_power6+lsu2_power6")

(define_reservation "FPU_power6"
                    "fpu1_power6|fpu2_power6")

(define_reservation "BRU_power6"
                    "bpu_power6")

(define_reservation "LSU_power6"
                    "lsu1_power6|lsu2_power6")

(define_reservation "LSF_power6"
                    "(lsu1_power6+fpu1_power6)\
                    |(lsu1_power6+fpu2_power6)\
                    |(lsu2_power6+fpu1_power6)\
                    |(lsu2_power6+fpu2_power6)")

(define_reservation "LX2_power6"
                    "(iu1_power6+iu2_power6+lsu1_power6)\
                    |(iu1_power6+iu2_power6+lsu2_power6)")

(define_reservation "FX2_power6"
                    "iu1_power6+iu2_power6")

(define_reservation "BX2_power6"
                    "iu1_power6+iu2_power6+bpu_power6")

(define_reservation "LSX_power6"
                    "(iu1_power6+lsu1_power6)\
                    |(iu1_power6+lsu2_power6)\
                    |(iu2_power6+lsu1_power6)\
                    |(iu2_power6+lsu2_power6)")

(define_reservation "FXU_power6"
                    "iu1_power6|iu2_power6")

(define_reservation "XLF_power6"
                    "(iu1_power6+lsu1_power6+fpu1_power6)\
                    |(iu1_power6+lsu1_power6+fpu2_power6)\
                    |(iu1_power6+lsu2_power6+fpu1_power6)\
                    |(iu1_power6+lsu2_power6+fpu2_power6)\
                    |(iu2_power6+lsu1_power6+fpu1_power6)\
                    |(iu2_power6+lsu1_power6+fpu2_power6)\
                    |(iu2_power6+lsu2_power6+fpu1_power6)\
                    |(iu2_power6+lsu2_power6+fpu2_power6)")

(define_reservation "BRX_power6"
                    "(bpu_power6+iu1_power6)\
                    |(bpu_power6+iu2_power6)")

; Load/store

; The default for a value written by a fixed point load
; that is read/written by a subsequent fixed point op.
(define_insn_reservation "power6-load" 2 ; fx
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "no")
       (eq_attr "update" "no")
       (eq_attr "cpu" "power6"))
  "LSU_power6")

; define the bypass for the case where the value written
; by a fixed point load is used as the source value on
; a store.
(define_bypass 1 "power6-load,\
                  power6-load-update,\
                  power6-load-update-indexed"
                 "power6-store,\
                  power6-store-update,\
                  power6-store-update-indexed,\
                  power6-fpstore,\
                  power6-fpstore-update"
  "rs6000_store_data_bypass_p")

(define_insn_reservation "power6-load-ext" 4 ; fx
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "yes")
       (eq_attr "update" "no")
       (eq_attr "cpu" "power6"))
  "LSU_power6")

; define the bypass for the case where the value written
; by a fixed point load ext is used as the source value on
; a store.
(define_bypass 1 "power6-load-ext,\
                  power6-load-ext-update,\
	          power6-load-ext-update-indexed"
                 "power6-store,\
                  power6-store-update,\
                  power6-store-update-indexed,\
                  power6-fpstore,\
                  power6-fpstore-update"
  "rs6000_store_data_bypass_p")

(define_insn_reservation "power6-load-update" 2 ; fx
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "no")
       (eq_attr "update" "yes")
       (eq_attr "indexed" "no")
       (eq_attr "cpu" "power6"))
  "LSX_power6")

(define_insn_reservation "power6-load-update-indexed" 2 ; fx
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "no")
       (eq_attr "update" "yes")
       (eq_attr "indexed" "yes")
       (eq_attr "cpu" "power6"))
  "LSX_power6")

(define_insn_reservation "power6-load-ext-update" 4 ; fx
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "yes")
       (eq_attr "update" "yes")
       (eq_attr "indexed" "no")
       (eq_attr "cpu" "power6"))
  "LSX_power6")

(define_insn_reservation "power6-load-ext-update-indexed" 4 ; fx
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "yes")
       (eq_attr "update" "yes")
       (eq_attr "indexed" "yes")
       (eq_attr "cpu" "power6"))
  "LSX_power6")

(define_insn_reservation "power6-fpload" 1
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "no")
       (eq_attr "cpu" "power6"))
  "LSU_power6")

(define_insn_reservation "power6-fpload-update" 1
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "yes")
       (eq_attr "cpu" "power6"))
  "LSX_power6")

(define_insn_reservation "power6-store" 14
  (and (eq_attr "type" "store")
       (eq_attr "update" "no")
       (eq_attr "cpu" "power6"))
  "LSU_power6")

(define_insn_reservation "power6-store-update" 14
  (and (eq_attr "type" "store")
       (eq_attr "update" "yes")
       (eq_attr "indexed" "no")
       (eq_attr "cpu" "power6"))
  "LSX_power6")

(define_insn_reservation "power6-store-update-indexed" 14
  (and (eq_attr "type" "store")
       (eq_attr "update" "yes")
       (eq_attr "indexed" "yes")
       (eq_attr "cpu" "power6"))
  "LX2_power6")

(define_insn_reservation "power6-fpstore" 14
  (and (eq_attr "type" "fpstore")
       (eq_attr "update" "no")
       (eq_attr "cpu" "power6"))
  "LSF_power6")

(define_insn_reservation "power6-fpstore-update" 14
  (and (eq_attr "type" "fpstore")
       (eq_attr "update" "yes")
       (eq_attr "cpu" "power6"))
  "XLF_power6")

(define_insn_reservation "power6-larx" 3
  (and (eq_attr "type" "load_l")
       (eq_attr "cpu" "power6"))
  "LS2_power6")

(define_insn_reservation "power6-stcx" 10 ; best case
  (and (eq_attr "type" "store_c")
       (eq_attr "cpu" "power6"))
  "LSX_power6")

(define_insn_reservation "power6-sync" 11 ; N/A
  (and (eq_attr "type" "sync")
       (eq_attr "cpu" "power6"))
  "LSU_power6")

(define_insn_reservation "power6-integer" 1
  (and (ior (eq_attr "type" "integer")
	    (and (eq_attr "type" "add,logical")
		 (eq_attr "dot" "no")))
       (eq_attr "cpu" "power6"))
  "FXU_power6")

(define_insn_reservation "power6-isel" 1
  (and (eq_attr "type" "isel")
       (eq_attr "cpu" "power6"))
  "FXU_power6")

(define_insn_reservation "power6-exts" 1
  (and (eq_attr "type" "exts")
       (eq_attr "dot" "no")
       (eq_attr "cpu" "power6"))
  "FXU_power6")

(define_insn_reservation "power6-shift" 1
  (and (eq_attr "type" "shift")
       (eq_attr "var_shift" "no")
       (eq_attr "dot" "no")
       (eq_attr "cpu" "power6"))
  "FXU_power6")

(define_insn_reservation "power6-popcnt" 1
  (and (eq_attr "type" "popcnt")
       (eq_attr "cpu" "power6"))
  "FXU_power6")

(define_insn_reservation "power6-insert" 1
  (and (eq_attr "type" "insert")
       (eq_attr "size" "32")
       (eq_attr "cpu" "power6"))
  "FX2_power6")

(define_insn_reservation "power6-insert-dword" 1
  (and (eq_attr "type" "insert")
       (eq_attr "size" "64")
       (eq_attr "cpu" "power6"))
  "FX2_power6")

; define the bypass for the case where the value written
; by a fixed point op is used as the source value on a
; store.
(define_bypass 1 "power6-integer,\
                  power6-exts,\
                  power6-shift,\
                  power6-insert,\
                  power6-insert-dword"
                 "power6-store,\
                  power6-store-update,\
                  power6-store-update-indexed,\
                  power6-fpstore,\
                  power6-fpstore-update"
  "rs6000_store_data_bypass_p")

(define_insn_reservation "power6-cntlz" 2
  (and (eq_attr "type" "cntlz")
       (eq_attr "cpu" "power6"))
  "FXU_power6")

(define_bypass 1 "power6-cntlz"
                 "power6-store,\
                  power6-store-update,\
                  power6-store-update-indexed,\
                  power6-fpstore,\
                  power6-fpstore-update"
  "rs6000_store_data_bypass_p")

(define_insn_reservation "power6-var-rotate" 4
  (and (eq_attr "type" "shift")
       (eq_attr "var_shift" "yes")
       (eq_attr "dot" "no")
       (eq_attr "cpu" "power6"))
  "FXU_power6")

(define_insn_reservation "power6-trap" 1 ; N/A
  (and (eq_attr "type" "trap")
       (eq_attr "cpu" "power6"))
  "BRX_power6")

(define_insn_reservation "power6-two" 1
  (and (eq_attr "type" "two")
       (eq_attr "cpu" "power6"))
  "(iu1_power6,iu1_power6)\
  |(iu1_power6+iu2_power6,nothing)\
  |(iu1_power6,iu2_power6)\
  |(iu2_power6,iu1_power6)\
  |(iu2_power6,iu2_power6)")

(define_insn_reservation "power6-three" 1
  (and (eq_attr "type" "three")
       (eq_attr "cpu" "power6"))
  "(iu1_power6,iu1_power6,iu1_power6)\
  |(iu1_power6,iu1_power6,iu2_power6)\
  |(iu1_power6,iu2_power6,iu1_power6)\
  |(iu1_power6,iu2_power6,iu2_power6)\
  |(iu2_power6,iu1_power6,iu1_power6)\
  |(iu2_power6,iu1_power6,iu2_power6)\
  |(iu2_power6,iu2_power6,iu1_power6)\
  |(iu2_power6,iu2_power6,iu2_power6)\
  |(iu1_power6+iu2_power6,iu1_power6)\
  |(iu1_power6+iu2_power6,iu2_power6)\
  |(iu1_power6,iu1_power6+iu2_power6)\
  |(iu2_power6,iu1_power6+iu2_power6)")

(define_insn_reservation "power6-cmp" 1
  (and (eq_attr "type" "cmp")
       (eq_attr "cpu" "power6"))
  "FXU_power6")

(define_insn_reservation "power6-compare" 1
  (and (eq_attr "type" "exts")
       (eq_attr "dot" "yes")
       (eq_attr "cpu" "power6"))
  "FXU_power6")

(define_insn_reservation "power6-fast-compare" 1
  (and (eq_attr "type" "add,logical")
       (eq_attr "dot" "yes")
       (eq_attr "cpu" "power6"))
  "FXU_power6")

; define the bypass for the case where the value written
; by a fixed point rec form op is used as the source value
; on a store.
(define_bypass 1 "power6-compare,\
                  power6-fast-compare"
                 "power6-store,\
                  power6-store-update,\
                  power6-store-update-indexed,\
                  power6-fpstore,\
                  power6-fpstore-update"
  "rs6000_store_data_bypass_p")

(define_insn_reservation "power6-delayed-compare" 2 ; N/A
  (and (eq_attr "type" "shift")
       (eq_attr "var_shift" "no")
       (eq_attr "dot" "yes")
       (eq_attr "cpu" "power6"))
  "FXU_power6")

(define_insn_reservation "power6-var-delayed-compare" 4
  (and (eq_attr "type" "shift")
       (eq_attr "var_shift" "yes")
       (eq_attr "dot" "yes")
       (eq_attr "cpu" "power6"))
  "FXU_power6")

(define_insn_reservation "power6-lmul-cmp" 16
  (and (eq_attr "type" "mul")
       (eq_attr "dot" "yes")
       (eq_attr "size" "64")
       (eq_attr "cpu" "power6"))
  "(iu1_power6*16+iu2_power6*16+fpu1_power6*16)\
  |(iu1_power6*16+iu2_power6*16+fpu2_power6*16)");

(define_insn_reservation "power6-imul-cmp" 16
  (and (eq_attr "type" "mul")
       (eq_attr "dot" "yes")
       (eq_attr "size" "32")
       (eq_attr "cpu" "power6"))
  "(iu1_power6*16+iu2_power6*16+fpu1_power6*16)\
  |(iu1_power6*16+iu2_power6*16+fpu2_power6*16)");

(define_insn_reservation "power6-lmul" 16
  (and (eq_attr "type" "mul")
       (eq_attr "dot" "no")
       (eq_attr "size" "64")
       (eq_attr "cpu" "power6"))
  "(iu1_power6*16+iu2_power6*16+fpu1_power6*16)\
  |(iu1_power6*16+iu2_power6*16+fpu2_power6*16)");

(define_insn_reservation "power6-imul" 16
  (and (eq_attr "type" "mul")
       (eq_attr "dot" "no")
       (eq_attr "size" "32")
       (eq_attr "cpu" "power6"))
  "(iu1_power6*16+iu2_power6*16+fpu1_power6*16)\
  |(iu1_power6*16+iu2_power6*16+fpu2_power6*16)");

(define_insn_reservation "power6-imul3" 16
  (and (eq_attr "type" "mul")
       (eq_attr "size" "8,16")
       (eq_attr "cpu" "power6"))
  "(iu1_power6*16+iu2_power6*16+fpu1_power6*16)\
  |(iu1_power6*16+iu2_power6*16+fpu2_power6*16)");

(define_bypass 9 "power6-imul,\
                  power6-lmul,\
                  power6-imul-cmp,\
                  power6-lmul-cmp,\
                  power6-imul3"
                 "power6-store,\
                  power6-store-update,\
                  power6-store-update-indexed,\
                  power6-fpstore,\
                  power6-fpstore-update"
  "rs6000_store_data_bypass_p")

(define_insn_reservation "power6-idiv" 44
  (and (eq_attr "type" "div")
       (eq_attr "size" "32")
       (eq_attr "cpu" "power6"))
  "(iu1_power6*44+iu2_power6*44+fpu1_power6*44)\
  |(iu1_power6*44+iu2_power6*44+fpu2_power6*44)");

; The latency for this bypass is yet to be defined
;(define_bypass ? "power6-idiv"
;                 "power6-store,\
;                  power6-store-update,\
;                  power6-store-update-indexed,\
;                  power6-fpstore,\
;                  power6-fpstore-update"
;  "rs6000_store_data_bypass_p")

(define_insn_reservation "power6-ldiv" 56
  (and (eq_attr "type" "div")
       (eq_attr "size" "64")
       (eq_attr "cpu" "power6"))
  "(iu1_power6*56+iu2_power6*56+fpu1_power6*56)\
  |(iu1_power6*56+iu2_power6*56+fpu2_power6*56)");

; The latency for this bypass is yet to be defined
;(define_bypass ? "power6-ldiv"
;                 "power6-store,\
;                  power6-store-update,\
;                  power6-store-update-indexed,\
;                  power6-fpstore,\
;                  power6-fpstore-update"
;  "rs6000_store_data_bypass_p")

(define_insn_reservation "power6-mtjmpr" 2
  (and (eq_attr "type" "mtjmpr,mfjmpr")
       (eq_attr "cpu" "power6"))
  "BX2_power6")

(define_bypass 5 "power6-mtjmpr" "power6-branch")

(define_insn_reservation "power6-branch" 2
  (and (eq_attr "type" "jmpreg,branch")
       (eq_attr "cpu" "power6"))
  "BRU_power6")

(define_bypass 5 "power6-branch" "power6-mtjmpr")

(define_insn_reservation "power6-crlogical" 3
  (and (eq_attr "type" "cr_logical")
       (eq_attr "cpu" "power6"))
  "BRU_power6")

(define_bypass 3 "power6-crlogical" "power6-branch")

(define_insn_reservation "power6-mfcr" 6 ; N/A
  (and (eq_attr "type" "mfcr")
       (eq_attr "cpu" "power6"))
  "BX2_power6")

; mfcrf (1 field)
(define_insn_reservation "power6-mfcrf" 3 ; N/A
  (and (eq_attr "type" "mfcrf")
       (eq_attr "cpu" "power6"))
  "BX2_power6") ;

; mtcrf (1 field)
(define_insn_reservation "power6-mtcr" 4 ; N/A
  (and (eq_attr "type" "mtcr")
       (eq_attr "cpu" "power6"))
  "BX2_power6")

(define_bypass 9 "power6-mtcr" "power6-branch")

(define_insn_reservation "power6-fp" 6
  (and (eq_attr "type" "fp,fpsimple,dmul,dfp")
       (eq_attr "cpu" "power6"))
  "FPU_power6")

; Any fp instruction that updates a CR has a latency
; of 6 to a dependent branch
(define_bypass 6 "power6-fp" "power6-branch")

(define_bypass 1 "power6-fp"
                 "power6-fpstore,power6-fpstore-update"
  "rs6000_store_data_bypass_p")

(define_insn_reservation "power6-fpcompare" 8
  (and (eq_attr "type" "fpcompare")
       (eq_attr "cpu" "power6"))
  "FPU_power6")

(define_bypass 12 "power6-fpcompare"
                  "power6-branch,power6-crlogical")

(define_insn_reservation "power6-sdiv" 26
  (and (eq_attr "type" "sdiv")
       (eq_attr "cpu" "power6"))
  "FPU_power6")

(define_insn_reservation "power6-ddiv" 32
  (and (eq_attr "type" "ddiv")
       (eq_attr "cpu" "power6"))
  "FPU_power6")

(define_insn_reservation "power6-sqrt" 30
  (and (eq_attr "type" "ssqrt")
       (eq_attr "cpu" "power6"))
  "FPU_power6")

(define_insn_reservation "power6-dsqrt" 42
  (and (eq_attr "type" "dsqrt")
       (eq_attr "cpu" "power6"))
  "FPU_power6")

(define_insn_reservation "power6-isync" 2 ; N/A 
  (and (eq_attr "type" "isync")
       (eq_attr "cpu" "power6"))
  "FXU_power6")

(define_insn_reservation "power6-vecload" 1
  (and (eq_attr "type" "vecload")
       (eq_attr "cpu" "power6"))
  "LSU_power6")

(define_insn_reservation "power6-vecstore" 1
  (and (eq_attr "type" "vecstore")
       (eq_attr "cpu" "power6"))
  "LSF_power6")

(define_insn_reservation "power6-vecsimple" 3
  (and (eq_attr "type" "vecsimple,veclogical,vecmove")
       (eq_attr "cpu" "power6"))
  "FPU_power6")

(define_bypass 6 "power6-vecsimple" "power6-veccomplex,\
                                     power6-vecperm")

(define_bypass 5 "power6-vecsimple" "power6-vecfloat")

(define_bypass 4 "power6-vecsimple" "power6-vecstore" )

(define_insn_reservation "power6-veccmp" 1
  (and (eq_attr "type" "veccmp,veccmpfx")
       (eq_attr "cpu" "power6"))
  "FPU_power6")

(define_bypass 10 "power6-veccmp" "power6-branch")

(define_insn_reservation "power6-vecfloat" 7
  (and (eq_attr "type" "vecfloat")
       (eq_attr "cpu" "power6"))
  "FPU_power6")

(define_bypass 10 "power6-vecfloat" "power6-vecsimple")

(define_bypass 11 "power6-vecfloat" "power6-veccomplex,\
                                     power6-vecperm")

(define_bypass 9 "power6-vecfloat" "power6-vecstore" )

(define_insn_reservation "power6-veccomplex" 7
  (and (eq_attr "type" "vecsimple")
       (eq_attr "cpu" "power6"))
  "FPU_power6")

(define_bypass 10 "power6-veccomplex" "power6-vecsimple,\
                                       power6-vecfloat" )

(define_bypass 9 "power6-veccomplex" "power6-vecperm" )

(define_bypass 8 "power6-veccomplex" "power6-vecstore" )

(define_insn_reservation "power6-vecperm" 4
  (and (eq_attr "type" "vecperm")
       (eq_attr "cpu" "power6"))
  "FPU_power6")

(define_bypass 7 "power6-vecperm" "power6-vecsimple,\
                                   power6-vecfloat" )

(define_bypass 6 "power6-vecperm" "power6-veccomplex" )

(define_bypass 5 "power6-vecperm" "power6-vecstore" )

