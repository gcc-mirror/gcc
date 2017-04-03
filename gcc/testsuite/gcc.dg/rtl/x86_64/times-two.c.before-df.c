/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-fdump-rtl-dfinit" } */

int __RTL (startwith ("rtl-dfinit")) times_two (int i)
{
  /* C function:
     return i * 2;  */
(function "times_two"
  (insn-chain
    (cnote 1 NOTE_INSN_DELETED)
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 4 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 2 (set (mem/c:SI (plus:DI (reg/f:DI frame)
                            (const_int -4)) [1 i+0 S4 A32])
                    (reg:SI di [ i ])) "../../src/times-two.c":2)
      (cnote 3 NOTE_INSN_FUNCTION_BEG)
      (cinsn 6 (set (reg:SI <2>)
                    (mem/c:SI (plus:DI (reg/f:DI frame)
                            (const_int -4)) [1 i+0 S4 A32])) "../../src/times-two.c":3)
      (cinsn 7 (parallel [
                        (set (reg:SI <0> [ _2 ])
                            (ashift:SI (reg:SI <2>)
                                (const_int 1)))
                        (clobber (reg:CC flags))
                    ]) "../../src/times-two.c":3
                 (expr_list:REG_EQUAL (ashift:SI (mem/c:SI (plus:DI (reg/f:DI frame)
                                (const_int -4)) [1 i+0 S4 A32])
                        (const_int 1))))
      (cinsn 10 (set (reg:SI <1> [ <retval> ])
                    (reg:SI <0> [ _2 ])) "../../src/times-two.c":3)
      (cinsn 14 (set (reg/i:SI ax)
                    (reg:SI <1> [ <retval> ])) "../../src/times-two.c":4)
      (cinsn 15 (use (reg/i:SI ax)) "../../src/times-two.c":4)
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
  (crtl
    (return_rtx 
      (reg/i:SI ax)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "times_two"
}

/* Verify that the dataflow information matches what cc1 would have
   generated.  In particular, in earlier versions of the RTL
   frontend, the exit block use of reg 0 (ax) wasn't picked up
   on, due to not setting up crtl->return_rtx based on
   DECL_RESULT (fndecl).  */

/* { dg-final { scan-rtl-dump ";;  exit block uses.*0 .ax. 6 .bp. 7 .sp. 20 .frame." "dfinit" } } */

/* { dg-final { scan-rtl-dump ";;  regs ever live.*0 .ax. 5 .di. 17 .flags." "dfinit" } } */
