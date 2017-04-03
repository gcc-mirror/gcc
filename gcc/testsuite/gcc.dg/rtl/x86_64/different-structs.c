/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && lp64 } } } */

extern double sqrt(double x);

struct foo
{
  double x;
  double y;
};

struct bar
{
  double x;
  double y;
};

double __RTL test (struct foo *f, const struct bar *b)
{
#if 0
  /* Result of "expand" on this C code, compiled for x86_64 with -Os.  */
  f->x += b->x;
  f->y += b->y;
  return sqrt (f->x * f->x + f->y * f->y);
#endif
(function "test"
  (insn-chain
    (cnote 1 NOTE_INSN_DELETED)
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 5 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 2 (set (reg/v/f:DI <10> [ f ])
                    (reg:DI di [ f ])) "../../src/gcc/testsuite/gcc.dg/rtl/x86_64/different-structs.c":18)
      (cinsn 3 (set (reg/v/f:DI <11> [ b ])
                    (reg:DI si [ b ])) "../../src/gcc/testsuite/gcc.dg/rtl/x86_64/different-structs.c":18)
      (cnote 4 NOTE_INSN_FUNCTION_BEG)
      (cinsn 7 (set (reg:DF <12>)
                    (mem:DF (reg/v/f:DI <10> [ f ]) [2 f_11(D)->x+0 S8 A64])) "../../src/gcc/testsuite/gcc.dg/rtl/x86_64/different-structs.c":21)
      (cinsn 8 (set (reg:DF <2> [ _3 ])
                    (plus:DF (reg:DF <12>)
                        (mem:DF (reg/v/f:DI <11> [ b ]) [2 b_12(D)->x+0 S8 A64]))) "../../src/gcc/testsuite/gcc.dg/rtl/x86_64/different-structs.c":21)
      (cinsn 9 (set (mem:DF (reg/v/f:DI <10> [ f ]) [2 f_11(D)->x+0 S8 A64])
                    (reg:DF <2> [ _3 ])) "../../src/gcc/testsuite/gcc.dg/rtl/x86_64/different-structs.c":21)
      (cinsn 10 (set (reg:DF <13>)
                    (mem:DF (plus:DI (reg/v/f:DI <10> [ f ])
                            (const_int 8)) [2 f_11(D)->y+0 S8 A64])) "../../src/gcc/testsuite/gcc.dg/rtl/x86_64/different-structs.c":22)
      (cinsn 11 (set (reg:DF <5> [ _6 ])
                    (plus:DF (reg:DF <13>)
                        (mem:DF (plus:DI (reg/v/f:DI <11> [ b ])
                                (const_int 8)) [2 b_12(D)->y+0 S8 A64]))) "../../src/gcc/testsuite/gcc.dg/rtl/x86_64/different-structs.c":22)
      (cinsn 12 (set (mem:DF (plus:DI (reg/v/f:DI <10> [ f ])
                            (const_int 8)) [2 f_11(D)->y+0 S8 A64])
                    (reg:DF <5> [ _6 ])) "../../src/gcc/testsuite/gcc.dg/rtl/x86_64/different-structs.c":22)
      (cinsn 13 (set (reg:DF <14>)
                    (mult:DF (reg:DF <2> [ _3 ])
                        (reg:DF <2> [ _3 ]))) "../../src/gcc/testsuite/gcc.dg/rtl/x86_64/different-structs.c":23)
      (cinsn 14 (set (reg:DF <15>)
                    (mult:DF (reg:DF <5> [ _6 ])
                        (reg:DF <5> [ _6 ]))) "../../src/gcc/testsuite/gcc.dg/rtl/x86_64/different-structs.c":23)
      (cinsn 15 (set (reg:DF <16>)
                    (plus:DF (reg:DF <14>)
                        (reg:DF <15>))) "../../src/gcc/testsuite/gcc.dg/rtl/x86_64/different-structs.c":23)
      (cinsn 16 (set (reg:DF xmm0)
                    (reg:DF <16>)) "../../src/gcc/testsuite/gcc.dg/rtl/x86_64/different-structs.c":23)
      (ccall_insn/j 17 (set (reg:DF xmm0)
                    (call (mem:QI (symbol_ref:DI ("sqrt") [flags 0x41]  <function_decl 0x7fa24e331d00 sqrt>) [0 __builtin_sqrt S1 A8])
                        (const_int 0))) "../../src/gcc/testsuite/gcc.dg/rtl/x86_64/different-structs.c":23
                 (expr_list:REG_CALL_DECL (symbol_ref:DI ("sqrt") [flags 0x41]  <function_decl 0x7fa24e331d00 sqrt>)
                    (expr_list:REG_EH_REGION (const_int 0)))
                (expr_list:DF (use (reg:DF xmm0))))
      (edge-to exit (flags "ABNORMAL | SIBCALL"))
    ) ;; block 2
    (cbarrier 18)
  ) ;; insn-chain
  (crtl
    (return_rtx 
      (reg/i:DF xmm0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "test"

}
