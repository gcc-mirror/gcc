/* { dg-do run { target { { i?86-*-* x86_64-*-* } && lp64 } } } */

/* Verify that we can have multiple __RTL functions in one test case.
   Each of these __RTL functions returns a const, dumped immediately after
   expand.  */

extern void abort (void);

int __RTL (startwith ("vregs")) test_return_42 (void)
{
  /* C code:
     return 42; */
(function "test_return_42"
  (insn-chain
    (cnote 1 NOTE_INSN_DELETED)
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 5 (set (reg:SI <0> [ _1 ])
                    (const_int 42)) "../../src/test-return-const.c":3)
      (cinsn 8 (set (reg:SI <1> [ <retval> ])
                    (reg:SI <0> [ _1 ])) "../../src/test-return-const.c":3)
      (cinsn 12 (set (reg/i:SI ax)
                    (reg:SI <1> [ <retval> ])) "../../src/test-return-const.c":4)
      (cinsn 13 (use (reg/i:SI ax)) "../../src/test-return-const.c":4)
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
  (crtl
    (return_rtx 
      (reg/i:SI ax)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "test_return_42"
}

int __RTL (startwith ("vregs")) test_return_43 (void)
{
  /* C code:
     return 43; */
(function "test_return_43"
  (insn-chain
    (cnote 1 NOTE_INSN_DELETED)
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 5 (set (reg:SI <0> [ _1 ])
                    (const_int 43)) "../../src/test-return-const.c":3)
      (cinsn 8 (set (reg:SI <1> [ <retval> ])
                    (reg:SI <0> [ _1 ])) "../../src/test-return-const.c":3)
      (cinsn 12 (set (reg/i:SI ax)
                    (reg:SI <1> [ <retval> ])) "../../src/test-return-const.c":4)
      (cinsn 13 (use (reg/i:SI ax)) "../../src/test-return-const.c":4)
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
  (crtl
    (return_rtx 
      (reg/i:SI ax)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "test_return_43"
}

int __RTL (startwith ("vregs")) test_return_44 (void)
{
  /* C code:
     return 44; */
(function "test_return_44"
  (insn-chain
    (cnote 1 NOTE_INSN_DELETED)
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 5 (set (reg:SI <0> [ _1 ])
                    (const_int 44)) "../../src/test-return-const.c":3)
      (cinsn 8 (set (reg:SI <1> [ <retval> ])
                    (reg:SI <0> [ _1 ])) "../../src/test-return-const.c":3)
      (cinsn 12 (set (reg/i:SI ax)
                    (reg:SI <1> [ <retval> ])) "../../src/test-return-const.c":4)
      (cinsn 13 (use (reg/i:SI ax)) "../../src/test-return-const.c":4)
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
  (crtl
    (return_rtx 
      (reg/i:SI ax)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "test_return_44"
}

int main (void)
{
  if (test_return_42 () != 42)
    abort ();
  if (test_return_43 () != 43)
    abort ();
  if (test_return_44 () != 44)
    abort ();
  return 0;
}
