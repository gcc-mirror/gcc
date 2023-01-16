;;  Machine Description for Renesas RL78 processors
;;  Copyright (C) 2011-2023 Free Software Foundation, Inc.
;;  Contributed by Red Hat.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; In this MD file, we define those insn patterns that involve
;; registers, where such registers are virtual until allocated to a
;; physical register.  All of these insns need to be conditional on
;; rl78_virt_insns_ok () being true.

;; This tells the physical register allocator what method to use to
;; allocate registers.  Basically, this defines the template of the
;; instruction - op1 is of the form "a = op(b)", op2 is "a = b op c"
;; etc.

(define_attr "valloc" "op1,op2,ro1,cmp,umul,macax,divhi,divsi"
  (const_string "op2"))

;;---------- Moving ------------------------

(define_insn "*movqi_virt_mm"
  [(set (match_operand:QI 0 "rl78_near_mem_operand" "=Y")
	(match_operand    1 "rl78_near_mem_operand" "Y"))]
  "rl78_virt_insns_ok ()"
  "v.mov %0, %1"
  [(set_attr "valloc" "op1")]
)

(define_insn "*movqi_virt"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=vY,v,*Wfr,Y,*Wfr,*Wfr")
	(match_operand    1 "general_operand" "vInt8JY,*Wfr,vInt8J,*Wfr,Y,*Wfr"))]
  "rl78_virt_insns_ok ()"
  "v.mov %0, %1"
  [(set_attr "valloc" "op1")]
)

(define_insn "*movhi_virt_mm"
  [(set (match_operand:HI 0 "rl78_near_mem_operand" "=Y")
	(match_operand:HI 1 "rl78_near_mem_operand" "Y"))]
  "rl78_virt_insns_ok ()"
  "v.movw %0, %1"
  [(set_attr "valloc" "op1")]
)

(define_insn "*movhi_virt"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=vS,  Y,   v,   *Wfr")
	(match_operand:HI 1 "general_operand"      "viYS, viS, *Wfr, vi"))]
  "rl78_virt_insns_ok ()"
  "v.movw %0, %1"
  [(set_attr "valloc" "op1")]
)

(define_insn "*bswaphi2_virt"
  [(set (match_operand:HI           0 "rl78_nonfar_nonimm_operand" "=vm")
        (bswap:HI (match_operand:HI 1 "general_operand"  "vim")))]
  "rl78_virt_insns_ok ()"
  "v.bswaphi\t%0, %1"
  [(set_attr "valloc" "op1")]
)

;;---------- Conversions ------------------------

(define_insn "*zero_extendqihi2_virt"
  [(set (match_operand:HI                 0 "rl78_nonfar_nonimm_operand" "=vY,*Wfr")
	(zero_extend:HI (match_operand:QI 1 "general_operand" "vim,viY")))]
  "rl78_virt_insns_ok () && rl78_one_far_p (operands, 2)"
  "v.zero_extend\t%0, %1"
  [(set_attr "valloc" "op1")]
  )

(define_insn "*extendqihi2_virt"
  [(set (match_operand:HI                 0 "rl78_nonfar_nonimm_operand" "=vY,*Wfr")
	(sign_extend:HI (match_operand:QI 1 "general_operand" "vim,viY")))]
  "rl78_virt_insns_ok () && rl78_one_far_p (operands, 2)"
  "v.sign_extend\t%0, %1"
  [(set_attr "valloc" "op1")]
  )

;;---------- Arithmetic ------------------------

(define_insn "*inc<mode>3_virt"
  [(set (match_operand:QHI           0 "rl78_incdec_memory_operand" "=vm")
	(plus:QHI (match_operand:QHI 1 "rl78_incdec_memory_operand" "0")
		  (match_operand:QHI 2 "rl78_1_2_operand" "KLNO")))
   ]
  "rl78_virt_insns_ok ()"
  "v.inc\t%0, %1, %2"
)

(define_insn "*add<mode>3_virt"
  [(set (match_operand:QHI           0 "rl78_nonimmediate_operand" "=vY,  S, *Wfr,  vY")
	(plus:QHI (match_operand:QHI 1 "rl78_general_operand"      "%viY, 0, 0viY, *Wfr")
		  (match_operand:QHI 2 "rl78_general_operand"       "vim, i, viY,  viY")))
   ]
  "rl78_virt_insns_ok () && rl78_one_far_p (operands, 3)"
  "v.add\t%0, %1, %2"
)

(define_insn "*sub<mode>3_virt"
  [(set (match_operand:QHI            0 "rl78_nonimmediate_operand" "=vY, S, *Wfr,  vY")
	(minus:QHI (match_operand:QHI 1 "rl78_general_operand"      "viY, 0, 0viY, *Wfr")
		   (match_operand:QHI 2 "rl78_general_operand"      "vim, i, viY,  viY")))
   ]
  "rl78_virt_insns_ok () && rl78_one_far_p (operands, 3)"
  "v.sub\t%0, %1, %2"
)

(define_insn "*umulhi3_shift_virt"
  [(set (match_operand:HI          0 "register_operand" "=v")
        (mult:HI (match_operand:HI 1 "rl78_nonfar_operand" "%vim")
                 (match_operand:HI 2 "rl78_24_operand" "Ni")))]
  "rl78_virt_insns_ok ()"
  "v.mulu\t%0, %1, %2"
  [(set_attr "valloc" "umul")]
)

(define_insn "*umulqihi3_virt"
  [(set (match_operand:HI                          0 "register_operand" "=v")
        (mult:HI (zero_extend:HI (match_operand:QI 1 "rl78_nonfar_operand" "%vim"))
                 (zero_extend:HI (match_operand:QI 2 "general_operand" "vim"))))]
  "rl78_virt_insns_ok ()"
  "v.mulu\t%0, %2"
  [(set_attr "valloc" "umul")]
)

(define_insn "*andqi3_virt"
  [(set (match_operand:QI         0 "rl78_nonimmediate_operand" "=vm,  *Wfr,  vY")
	(and:QI (match_operand:QI 1 "rl78_general_operand"      "%vim, 0viY, *Wfr")
		(match_operand:QI 2 "rl78_general_operand"      "vim,  viY,  viY")))
   ]
  "rl78_virt_insns_ok () && rl78_one_far_p (operands, 3)"
  "v.and\t%0, %1, %2"
)

(define_insn "*iorqi3_virt"
  [(set (match_operand:QI         0 "rl78_nonimmediate_operand" "=vm,  *Wfr,  vY")
	(ior:QI (match_operand:QI 1 "rl78_general_operand"      "%vim, 0viY, *Wfr")
		(match_operand:QI 2 "rl78_general_operand"      "vim,  viY,  viY")))
   ]
  "rl78_virt_insns_ok () && rl78_one_far_p (operands, 3)"
  "v.or\t%0, %1, %2"
)

(define_insn "*xorqi3_virt"
  [(set (match_operand:QI         0 "rl78_nonimmediate_operand" "=vm,  *Wfr,  vY")
	(xor:QI (match_operand:QI 1 "rl78_general_operand"      "%vim, 0viY, *Wfr")
		(match_operand    2 "rl78_general_operand"      "vim,  viY,  viY")))
   ]
  "rl78_virt_insns_ok () && rl78_one_far_p (operands, 3)"
  "v.xor\t%0, %1, %2"
)

;;---------- Shifts ------------------------

(define_insn "*ashl<mode>3_virt"
  [(set (match_operand:QHI             0 "rl78_nonfar_nonimm_operand" "=vm")
	(ashift:QHI (match_operand:QHI 1 "rl78_nonfar_operand" "vim")
		    (match_operand:QI  2 "general_operand" "vim")))
   ]
  "rl78_virt_insns_ok ()"
  "v.shl\t%0, %1, %2"
)

(define_insn "*ashr<mode>3_virt"
  [(set (match_operand:QHI               0 "rl78_nonfar_nonimm_operand" "=vm")
	(ashiftrt:QHI (match_operand:QHI 1 "rl78_nonfar_operand" "vim")
		      (match_operand:QI  2 "general_operand" "vim")))
   ]
  "rl78_virt_insns_ok ()"
  "v.sar\t%0, %1, %2"
)

(define_insn "*lshr<mode>3_virt"
  [(set (match_operand:QHI               0 "rl78_nonfar_nonimm_operand" "=vm")
	(lshiftrt:QHI (match_operand:QHI 1 "rl78_nonfar_operand" "vim")
		      (match_operand:QI  2 "general_operand" "vim")))
   ]
  "rl78_virt_insns_ok ()"
  "v.shr\t%0, %1, %2"
)

;; This is complex mostly because the RL78 has no SImode operations,
;; and very limited HImode operations, and no variable shifts.  This
;; pattern is optimized for each constant shift count and operand
;; types, so as to use a hand-optimized pattern.  For readability, the
;; usual \t\; syntax is not used here.  Also, there's no easy way to 
;; constrain to avoid partial overlaps, hence the duplication.
(define_insn "ashrsi3_virt"                                  ;;   0  1      2-7            8         9-15           16   17-23     24   25-31 var
  [(set (match_operand:SI               0 "nonimmediate_operand" "=v,vU,&vU,v,  &vU,  &vU, v,  &vU,  v,  &vU, &vU,  vU,  v,&vU,    vU,  vU,   vU")
	(ashiftrt:SI (match_operand:SI  1 "nonimmediate_operand" "0, 0,  vU,0,   vWab, U,  0,   vU,  0,   vWab,U,   vU,  0, vU,    vU,  vU,   0")
		      (match_operand:SI 2 "nonmemory_operand"    "M, K,  K, Int3,Int3,Int3,Iv08,Iv08,Is09,Is09,Is09,Iv16,Is17,Is17,Iv24,Is25, iv")))
   (clobber (reg:HI X_REG))
    ]
   ""
   "@
    ; ashrsi %0, 0

   movw ax,%H1 \; sarw ax,1 \; movw %H0,ax \; mov a,%Q1 \; rorc a,1 \; mov %Q0,a \; mov a,%q1 \; rorc a,1 \; mov %q0,a
   movw ax,%H1 \; sarw ax,1 \; movw %H0,ax \; mov a,%Q1 \; rorc a,1 \; mov %Q0,a \; mov a,%q1 \; rorc a,1 \; mov %q0,a

   movw ax,%1 \; shlw ax,%r2 \; mov %0,a             \; mov x,%Q1 \; mov a,%H1 \; shlw ax,%r2 \; mov %Q0,a \; movw ax,%H1 \; sarw ax,%u2 \; movw %H0,ax
   movw ax,%1 \; shlw ax,%r2 \; mov %0,a             \; mov x,%Q1 \; mov a,%H1 \; shlw ax,%r2 \; mov %Q0,a \; movw ax,%H1 \; sarw ax,%u2 \; movw %H0,ax
   movw ax,%1 \; shlw ax,%r2 \; mov %0,a \; mov a,%Q1 \; mov x,a   \; mov a,%H1 \; shlw ax,%r2 \; mov %Q0,a \; movw ax,%H1 \; sarw ax,%u2 \; movw %H0,ax

   mov x,%Q1            \; mov a,%H1 \; movw %0,ax \; movw ax,%H1 \; sarw ax,8 \; movw %H0,ax
   mov a,%Q1 \; mov x, a \; mov a,%H1 \; movw %0,ax \; movw ax,%H1 \; sarw ax,8 \; movw %H0,ax

   mov x,%Q1           \; mov a,%H1 \; shlw ax,%r2 \; mov %0,a \; movw ax,%H1 \; shlw ax,%r2 \; mov %Q0,a \; movw ax,%H1 \; sarw ax,%u2 \; movw %H0,ax
   mov x,%Q1           \; mov a,%H1 \; shlw ax,%r2 \; mov %0,a \; movw ax,%H1 \; shlw ax,%r2 \; mov %Q0,a \; movw ax,%H1 \; sarw ax,%u2 \; movw %H0,ax
   mov a,%Q1 \; mov x,a \; mov a,%H1 \; shlw ax,%r2 \; mov %0,a \; movw ax,%H1 \; shlw ax,%r2 \; mov %Q0,a \; movw ax,%H1 \; sarw ax,%u2 \; movw %H0,ax

   movw ax,%H1 \; movw %0,ax \; sarw ax,15 \; movw %H0,ax

   movw ax,%H1 \; sarw ax,%S2 \; movw %0,ax \; sarw ax,15 \; movw %H0,ax
   movw ax,%H1 \; sarw ax,%S2 \; movw %0,ax \; sarw ax,15 \; movw %H0,ax

   movw ax,%H1 \; mov %0,a \; sarw ax,15 \; movw %H0,ax \; mov %Q0,a

   movw ax,%H1 \; sar a,%s2 \; mov %0,a \; sarw ax,15 \; movw %H0,ax \; mov %Q0,a

   mov b,%2 \; cmp0 b \; bz $2f \; 1: \; movw ax,%H1 \; sarw ax,1 \; movw %H0,ax \; mov a,%Q1 \; rorc a,1 \; mov %Q0,a \; mov a,%q1 \; rorc a,1 \; mov %q0,a \; dec b \; bnz $1b \; 2:"
  [(set_attr "valloc" "macax")]
)

;; Likewise.
(define_insn "lshrsi3_virt"                                  ;;   0  1      2-7            8         9-15           16   17-23     24   25-31 var
  [(set (match_operand:SI               0 "nonimmediate_operand" "=v,vU,&vU,v,  &vU,  &vU, v,  &vU,  v,  &vU, &vU,  vU,  v,&vU,    vU,  vU,   vU")
	(lshiftrt:SI (match_operand:SI  1 "nonimmediate_operand" "0, 0,  vU,0,   vWab, U,  0,   vU,  0,   vWab,U,   vU,  0, vU,    vU,  vU,   0")
		      (match_operand:SI 2 "nonmemory_operand"    "M, K,  K, Int3,Int3,Int3,Iv08,Iv08,Is09,Is09,Is09,Iv16,Is17,Is17,Iv24,Is25, iv")))
   (clobber (reg:HI X_REG))
   ]
  ""
  "@
   ; lshrsi %0, 0

   movw ax,%H1 \; shrw ax,1 \; movw %H0,ax \; mov a,%Q1 \; rorc a,1 \; mov %Q0,a \; mov a,%q1 \; rorc a,1 \; mov %q0,a
   movw ax,%H1 \; shrw ax,1 \; movw %H0,ax \; mov a,%Q1 \; rorc a,1 \; mov %Q0,a \; mov a,%q1 \; rorc a,1 \; mov %q0,a

   movw ax,%1 \; shlw ax,%r2 \; mov %0,a             \; mov x,%Q1 \; mov a,%H1 \; shlw ax,%r2 \; mov %Q0,a \; movw ax,%H1 \; shrw ax,%u2 \; movw %H0,ax
   movw ax,%1 \; shlw ax,%r2 \; mov %0,a             \; mov x,%Q1 \; mov a,%H1 \; shlw ax,%r2 \; mov %Q0,a \; movw ax,%H1 \; shrw ax,%u2 \; movw %H0,ax
   movw ax,%1 \; shlw ax,%r2 \; mov %0,a \; mov a,%Q1 \; mov x,a   \; mov a,%H1 \; shlw ax,%r2 \; mov %Q0,a \; movw ax,%H1 \; shrw ax,%u2 \; movw %H0,ax

   mov x,%Q1            \; mov a,%H1 \; movw %0,ax \; movw ax,%H1 \; shrw ax,8 \; movw %H0,ax
   mov a,%Q1 \; mov x, a \; mov a,%H1 \; movw %0,ax \; movw ax,%H1 \; shrw ax,8 \; movw %H0,ax

   mov x,%Q1           \; mov a,%H1 \; shlw ax,%r2 \; mov %0,a \; movw ax,%H1 \; shlw ax,%r2 \; mov %Q0,a \; movw ax,%H1 \; shrw ax,%u2 \; movw %H0,ax
   mov x,%Q1           \; mov a,%H1 \; shlw ax,%r2 \; mov %0,a \; movw ax,%H1 \; shlw ax,%r2 \; mov %Q0,a \; movw ax,%H1 \; shrw ax,%u2 \; movw %H0,ax
   mov a,%Q1 \; mov x,a \; mov a,%H1 \; shlw ax,%r2 \; mov %0,a \; movw ax,%H1 \; shlw ax,%r2 \; mov %Q0,a \; movw ax,%H1 \; shrw ax,%u2 \; movw %H0,ax

   movw ax,%H1 \; movw %0,ax \; movw ax,#0 \; movw %H0,ax

   movw ax,%H1 \; shrw ax,%S2 \; movw %0,ax \; movw ax,#0 \; movw %H0,ax
   movw ax,%H1 \; shrw ax,%S2 \; movw %0,ax \; movw ax,#0 \; movw %H0,ax

   movw ax,%H1 \; mov %0,a \; movw ax,#0 \; movw %H0,ax \; mov %Q0,a

   movw ax,%H1 \; shr a,%s2 \; mov %0,a \; movw ax,#0 \; movw %H0,ax \; mov %Q0,a

   mov b,%2 \; cmp0 b \; bz $2f \; 1: \; movw ax,%H1 \; shrw ax,1 \; movw %H0,ax \; mov a,%Q1 \; rorc a,1 \; mov %Q0,a \; mov a,%q1 \; rorc a,1 \; mov %q0,a \; dec b \; bnz $1b \; 2:"
  [(set_attr "valloc" "macax")]
)

;; Likewise.
(define_insn "ashlsi3_virt"                                ;;   0  1      2-7            8         9-15           16        17-23     24        25-31     var
  [(set (match_operand:SI             0 "nonimmediate_operand" "=v,vU,&vU,v,  &vU,  &vU, v,  &vU,  v,  &vU, &vU,  v,   U,   v,&vU,    v,   U,   v,   U,   vWab,vU,  vU")
	(ashift:SI (match_operand:SI  1 "nonimmediate_operand" "0, 0,  vU,0,   vWab, U,  0,   vU,  0,   vWab,U,   vU,  vU,  0, vU,    vU,  vU,  vU,  vU,  0,   vWab,U")
		    (match_operand:SI 2 "nonmemory_operand"    "M, K,  K, Int3,Int3,Int3,Iv08,Iv08,Is09,Is09,Is09,Iv16,Iv16,Is17,Is17,Iv24,Iv24,Is25,Is25,iv,  iv,  iv")))
   (clobber (reg:HI X_REG))
   ]
  ""
  "@
   ; lshrsi %0, 0

   movw ax,%1 \; shlw ax,1 \; movw %0,ax \; movw ax,%H1 \; rolwc ax,1 \; movw %H0,ax
   movw ax,%1 \; shlw ax,1 \; movw %0,ax \; movw ax,%H1 \; rolwc ax,1 \; movw %H0,ax

   movw ax,%H1 \; shlw ax,%u2 \; mov %E0,a \; mov x,%Q1           \; mov a, %H1 \; shlw ax,%S2 \; mov %H0,a \; movw ax,%1 \; shlw ax,%u2 \; movw %0,ax
   movw ax,%H1 \; shlw ax,%u2 \; mov %E0,a \; mov x,%Q1           \; mov a, %H1 \; shlw ax,%S2 \; mov %H0,a \; movw ax,%1 \; shlw ax,%u2 \; movw %0,ax
   movw ax,%H1 \; shlw ax,%u2 \; mov %E0,a \; mov a,%Q1 \; mov x,a \; mov a, %H1 \; shlw ax,%S2 \; mov %H0,a \; movw ax,%1 \; shlw ax,%u2 \; movw %0,ax

   mov x,%Q1           \; mov a,%H1 \; movw %H0,ax \; movw ax,%1 \; shlw ax,8 \; movw %0,ax
   mov a,%Q1 \; mov x,a \; mov a,%H1 \; movw %H0,ax \; movw ax,%1 \; shlw ax,8 \; movw %0,ax

   mov x,%Q1           \; mov a,%H1 \; shlw ax,%s2 \; movw %H0,ax \; movw ax,%1 \; shlw ax,%s2 \; mov %H0,a \; movw ax,%1 \; shlw ax,%u2 \; movw %0,ax
   mov x,%Q1           \; mov a,%H1 \; shlw ax,%s2 \; movw %H0,ax \; movw ax,%1 \; shlw ax,%s2 \; mov %H0,a \; movw ax,%1 \; shlw ax,%u2 \; movw %0,ax
   mov a,%Q1 \; mov x,a \; mov a,%H1 \; shlw ax,%s2 \; movw %H0,ax \; movw ax,%1 \; shlw ax,%s2 \; mov %H0,a \; movw ax,%1 \; shlw ax,%u2 \; movw %0,ax

   movw ax,%1 \; movw %H0,ax \; movw %0,#0
   movw ax,%1 \; movw %H0,ax \; movw ax,#0 \; movw %0,ax

   movw ax,%1 \; shlw ax,%S2 \; movw %H0,ax \; movw %0,#0
   movw ax,%1 \; shlw ax,%S2 \; movw %H0,ax \; movw ax,#0 \; movw %0,ax

   mov a,%1 \; movw %H0,ax \; mov %H0,#0 \; movw %0,#0
   mov a,%1 \; movw %H0,ax \; movw ax,#0 \; mov %H0,a \; movW %0,ax

   mov a,%1 \; shl a,%s2 \; movw %H0,ax \; mov %H0,#0 \; movw %0,#0
   mov a,%1 \; shl a,%s2 \; movw %H0,ax \; movw ax,#0 \; mov %H0,a \; movW %0,ax

   mov a,%2 \; cmp0 a \; bz $2f \; mov d,a \; movw ax,%H1 \; movw bc,%1 \; 1: \; shlw bc,1 \; rolwc ax,1 \; dec d \; bnz $1b \; movw %H0,ax \; movw ax,bc \; movw %0,ax \; 2:
   mov a,%2 \; mov d,a \; movw ax,%H1 \; movw bc,%1 \; cmp0 0xFFEFD \; bz $2f \; 1: \; shlw bc,1 \; rolwc ax,1 \; dec d \; bnz $1b \; 2: \; movw %H0,ax \; movw ax,bc \; movw %0,ax
   mov a,%2 \; mov d,a \; movw ax,%1 \; movw bc,ax \; movw ax,%H1 \; cmp0 0xFFEFD \; bz $2f \; 1: \; shlw bc,1 \; rolwc ax,1 \; dec d \; bnz $1b \; 2: \; movw %H0,ax \; movw ax,bc \; movw %0,ax"
   [(set_attr "valloc" "macax")]
 )

;;---------- Branching ------------------------

(define_insn "*indirect_jump_virt"
  [(set (pc)
	(match_operand:HI 0 "nonimmediate_operand" "vm"))]
  "rl78_virt_insns_ok ()"
  "v.br\t%0"
  [(set_attr "valloc" "ro1")]
)

(define_insn "*call_virt"
  [(call (match_operand:HI 0 "memory_operand" "Wab,Wcv")
	 (match_operand 1 "" ""))]
  "rl78_virt_insns_ok ()"
  "v.call\t%0"
  [(set_attr "valloc" "ro1")]
  )

(define_insn "*call_value_virt"
  [(set (match_operand 0 "register_operand" "=v,v")
	(call (match_operand:HI 1 "memory_operand" "Wab,Wcv")
	      (match_operand 2 "" "")))]
  "rl78_virt_insns_ok ()"
  "v.call\t%1"
  [(set_attr "valloc" "op1")]
  )

(define_insn "*cbranchqi4_virt_signed"
  [(set (pc) (if_then_else
	      (match_operator 0 "rl78_cmp_operator_signed"
			      [(match_operand:QI 1 "general_operand" "vim")
			       (match_operand:QI 2 "nonmemory_operand" "vi")])
              (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "rl78_virt_insns_ok ()"
  "v.cmp\t%1, %2\\n\tv.b%C0\t%3"
  [(set_attr "valloc" "cmp")]
  )

(define_insn "*cbranchqi4_virt"
  [(set (pc) (if_then_else
	      (match_operator 0 "rl78_cmp_operator_real"
			      [(match_operand:QI 1 "rl78_general_operand" "vim")
			       (match_operand:QI 2 "rl78_general_operand" "vim")])
              (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "rl78_virt_insns_ok ()"
  "v.cmp\t%1, %2\\n\tv.b%C0\t%3"
  [(set_attr "valloc" "cmp")]
  )

(define_insn "*cbranchhi4_virt_signed"
  [(set (pc) (if_then_else
	      (match_operator 0 "rl78_cmp_operator_signed"
			      [(match_operand:HI 1 "general_operand" "vim")
			       (match_operand:HI 2 "nonmemory_operand" "vi")])
              (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "rl78_virt_insns_ok ()"
  "v.cmpw\t%1, %2\\n\tv.b%C0\t%3"
  [(set_attr "valloc" "cmp")]
  )

(define_insn "*cbranchhi4_virt"
  [(set (pc) (if_then_else
	      (match_operator 0 "rl78_cmp_operator_real"
			      [(match_operand:HI 1 "rl78_general_operand" "vim")
			       (match_operand:HI 2 "rl78_general_operand" "vim")])
              (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "rl78_virt_insns_ok ()"
  "v.cmpw\t%1, %2\\n\tv.b%C0\t%3"
  [(set_attr "valloc" "cmp")]
  )

(define_insn "*cbranchsi4_virt"
  [(set (pc) (if_then_else
	      (match_operator 0 "rl78_cmp_operator"
			      [(match_operand:SI 1 "general_operand" "vim")
			       (match_operand:SI 2 "nonmemory_operand" "vi")])
              (label_ref (match_operand 3 "" ""))
	      (pc)))
   (clobber (reg:HI AX_REG))
   ]
  "rl78_virt_insns_ok ()"
  "v.cmpd\t%1, %2\\n\tv.b%C0\t%3"
  [(set_attr "valloc" "macax")]
  )

;;---------- Peepholes ------------------------

(define_peephole2
  [(set (match_operand:QI 0 "" "")
	(match_operand:QI 1 "" ""))
   (set (match_operand:QI 2 "" "")
	(match_operand:QI 3 "" ""))]
  "rl78_peep_movhi_p (operands)"
  [(set (match_dup 4)
	(match_dup 5))]
  "rl78_setup_peep_movhi (operands);"
  )

(define_peephole2
  [(set (reg:QI A_REG)
	(match_operand:QI 1 "" ""))
   (set (match_operand:QI 0 "" "")
	(reg:QI A_REG))
   (set (reg:QI A_REG)
	(match_operand:QI 3 "" ""))
   (set (match_operand:QI 2 "" "")
	(reg:QI A_REG))
   ]
  "rl78_peep_movhi_p (operands)"
  [(set (reg:HI AX_REG)
	(match_dup 5))
   (set (match_dup 4)
	(reg:HI AX_REG))
   ]
  "rl78_setup_peep_movhi (operands);"
  )

(define_insn "*negandhi3_virt"
  [(set (match_operand:HI                 0 "register_operand" "=v")
	(and:HI (neg:HI (match_operand:HI 1 "register_operand"  "0"))
 		(match_operand:HI         2 "immediate_operand" "n")))
   ]
  "rl78_virt_insns_ok ()"
  "v.nand\t%0, %1, %2"
)
