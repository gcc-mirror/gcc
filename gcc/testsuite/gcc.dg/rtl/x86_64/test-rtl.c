/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && lp64 } } } */

/* Test of embedding RTL dump in a C function, tagged with "__RTL".

   This is a dump of test.c from immediately after "expand", for x86_64.  */

int __RTL test_1 (int i, int j, int k)
{
  /*
    if (i < j)
      return k + 4;
    else
      return -k;
  */
(function "test_1"
  (insn-chain
    (cnote 1 NOTE_INSN_DELETED)
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 6 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 2 (set (mem/c:SI (plus:DI (reg/f:DI virtual-stack-vars)
                            (const_int -4)) [1 i+0 S4 A32])
                    (reg:SI di [ i ])) "../../src/gcc/testsuite/gcc.dg/rtl/test.c":2)
      (cinsn 3 (set (mem/c:SI (plus:DI (reg/f:DI virtual-stack-vars)
                            (const_int -8)) [1 j+0 S4 A32])
                    (reg:SI si [ j ])) "../../src/gcc/testsuite/gcc.dg/rtl/test.c":2)
      (cinsn 4 (set (mem/c:SI (plus:DI (reg/f:DI virtual-stack-vars)
                            (const_int -12)) [1 k+0 S4 A32])
                    (reg:SI dx [ k ])) "../../src/gcc/testsuite/gcc.dg/rtl/test.c":2)
      (cnote 5 NOTE_INSN_FUNCTION_BEG)
      (cinsn 8 (set (reg:SI <2>)
                    (mem/c:SI (plus:DI (reg/f:DI virtual-stack-vars)
                            (const_int -4)) [1 i+0 S4 A32])) "../../src/gcc/testsuite/gcc.dg/rtl/test.c":3)
      (cinsn 9 (set (reg:CCGC flags)
                    (compare:CCGC (reg:SI <2>)
                        (mem/c:SI (plus:DI (reg/f:DI virtual-stack-vars)
                                (const_int -8)) [1 j+0 S4 A32]))) "../../src/gcc/testsuite/gcc.dg/rtl/test.c":3)
      (cjump_insn 10 (set (pc)
                    (if_then_else (ge (reg:CCGC flags)
                            (const_int 0))
                        (label_ref 16)
                        (pc))) "../../src/gcc/testsuite/gcc.dg/rtl/test.c":3)
      (edge-to 4 (flags "FALLTHRU"))
      (edge-to 5)
    ) ;; block 2
    (block 4
      (edge-from 2 (flags "FALLTHRU"))
      (cnote 11 [bb 4] NOTE_INSN_BASIC_BLOCK)
      (cinsn 12 (set (reg:SI <3>)
                    (mem/c:SI (plus:DI (reg/f:DI virtual-stack-vars)
                            (const_int -12)) [1 k+0 S4 A32])) "../../src/gcc/testsuite/gcc.dg/rtl/test.c":4)
      (cinsn 13 (parallel [
                        (set (reg:SI <0> [ _1 ])
                            (plus:SI (reg:SI <3>)
                                (const_int 4)))
                        (clobber (reg:CC flags))
                    ]) "../../src/gcc/testsuite/gcc.dg/rtl/test.c":4
                 (expr_list:REG_EQUAL (plus:SI (mem/c:SI (plus:DI (reg/f:DI virtual-stack-vars)
                                (const_int -12)) [1 k+0 S4 A32])
                        (const_int 4))))
      (cjump_insn 14 (set (pc)
                    (label_ref 20)) "../../src/gcc/testsuite/gcc.dg/rtl/test.c":4)
      (edge-to 6)
    ) ;; block 4
    (cbarrier 15)
    (block 5
      (edge-from 2)
      (clabel 16 2)
      (cnote 17 [bb 5] NOTE_INSN_BASIC_BLOCK)
      (cinsn 18 (set (reg:SI <4>)
                    (mem/c:SI (plus:DI (reg/f:DI virtual-stack-vars)
                            (const_int -12)) [1 k+0 S4 A32])) "../../src/gcc/testsuite/gcc.dg/rtl/test.c":6)
      (cinsn 19 (parallel [
                        (set (reg:SI <0> [ _1 ])
                            (neg:SI (reg:SI <4>)))
                        (clobber (reg:CC flags))
                    ]) "../../src/gcc/testsuite/gcc.dg/rtl/test.c":6
                 (expr_list:REG_EQUAL (neg:SI (mem/c:SI (plus:DI (reg/f:DI virtual-stack-vars)
                                (const_int -12)) [1 k+0 S4 A32]))))
      (edge-to 6 (flags "FALLTHRU"))
    ) ;; block 5
    (block 6
      (edge-from 4)
      (edge-from 5 (flags "FALLTHRU"))
      (clabel 20 3)
      (cnote 21 [bb 6] NOTE_INSN_BASIC_BLOCK)
      (cinsn 22 (set (reg:SI <1> [ <retval> ])
                    (reg:SI <0> [ _1 ])))
      (cinsn 26 (set (reg/i:SI ax)
                    (reg:SI <1> [ <retval> ])) "../../src/gcc/testsuite/gcc.dg/rtl/test.c":7)
      (cinsn 27 (use (reg/i:SI ax)) "../../src/gcc/testsuite/gcc.dg/rtl/test.c":7)
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 6
  ) ;; insn-chain
  (crtl
    (return_rtx 
      (reg/i:SI ax)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "test_1"
}
