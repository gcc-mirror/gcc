;; S390 TPF-OS specific machine patterns
;; Copyright (C) 2005, 2007 Free Software Foundation, Inc.
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

(define_insn "prologue_tpf"
  [(unspec_volatile [(const_int 0)] UNSPECV_TPF_PROLOGUE)
   (clobber (reg:DI 1))]
  "TARGET_TPF_PROFILING"
  "larl\t%%r1,.+14\;tm\t4065,255\;bnz\t4064"
  [(set_attr "length"   "14")])


(define_insn "epilogue_tpf"
  [(unspec_volatile [(const_int 0)] UNSPECV_TPF_EPILOGUE)
   (clobber (reg:DI 1))]
  "TARGET_TPF_PROFILING"
  "larl\t%%r1,.+14\;tm\t4071,255\;bnz\t4070"
  [(set_attr "length"   "14")])
