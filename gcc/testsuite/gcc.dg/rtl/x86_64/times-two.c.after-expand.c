/* { dg-do run { target { { i?86-*-* x86_64-*-* } && lp64 } } } */

extern void abort (void);

int __RTL (startwith ("vregs")) times_two (int i)
{
  /* C function:
     return i * 2;  */
(function "times_two"
  (param "i"
    (DECL_RTL (mem/c:SI (plus:DI (reg/f:DI virtual-stack-vars)
                    (const_int -4)) [1 i+0 S4 A32]))
    (DECL_RTL_INCOMING (reg:SI di [ i ]))
  ) ;; param "i"
  (insn-chain
    (cnote 1 NOTE_INSN_DELETED)
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 4 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 2 (set (mem/c:SI (plus:DI (reg/f:DI virtual-stack-vars)
                            (const_int -4)) [1 i+0 S4 A32])
                    (reg:SI di [ i ])) "../../src/times-two.c":2
                 (nil))
      (cnote 3 NOTE_INSN_FUNCTION_BEG)
      (cinsn 6 (set (reg:SI <2>)
                    (mem/c:SI (plus:DI (reg/f:DI virtual-stack-vars)
                            (const_int -4)) [1 i+0 S4 A32])) "../../src/times-two.c":3
                 (nil))
      (cinsn 7 (parallel [
                        (set (reg:SI <0> [ _2 ])
                            (ashift:SI (reg:SI <2>)
                                (const_int 1)))
                        (clobber (reg:CC flags))
                    ]) "../../src/times-two.c":3
                 (expr_list:REG_EQUAL (ashift:SI (mem/c:SI (plus:DI (reg/f:DI virtual-stack-vars)
                                (const_int -4)) [1 i+0 S4 A32])
                        (const_int 1))
                    (nil)))
      (cinsn 10 (set (reg:SI <1> [ <retval> ])
                    (reg:SI <0> [ _2 ])) "../../src/times-two.c":3
                 (nil))
      (cinsn 14 (set (reg/i:SI ax)
                    (reg:SI <1> [ <retval> ])) "../../src/times-two.c":4
                 (nil))
      (cinsn 15 (use (reg/i:SI ax)) "../../src/times-two.c":4
                 (nil))
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

int main (void)
{
  if (times_two (0) != 0)
    abort ();

  if (times_two (1) != 2)
    abort ();

  if (times_two (100) != 200)
    abort ();

  return 0;
}
