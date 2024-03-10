/* { dg-do compile { target powerpc*-*-linux* } } */
/* { dg-options "-O2 -mregnames" } */
/* { dg-require-effective-target has_arch_ppc64 } */

/* Following instruction sequence is found in assembly of
   Perl_block_start, which is a function of op.c in SPEC2017
   perlbench.  It can be never combined to a move and compare
   instruction in combine pass.  A peephole pattern is needed to
   converted the sequence to a "mr." instruction.

	cmpdi 0,9,0
	mr 12,9

   This test case is an analogue of the source code and verifies
   if the peephole2 patterns work.
*/

int __RTL (startwith ("peephole2")) compare_move_peephole ()
{
(function "compare_move_peephole"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 8 (set (reg:CC %cr0)
                    (compare:CC (reg:DI %r3)
                        (const_int 0))))
      (cinsn 2 (set (reg:DI %r4)
                    (reg:DI %r3)))
      ;; Extra insn to avoid the above being deleted by DCE.
      (cinsn 18 (use (reg:DI %r4)))
      (cinsn 19 (use (reg:CC %cr0)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function "main"
}

int __RTL (startwith ("peephole2")) move_compare_peephole ()
{
(function "move_compare_peephole"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 2 (set (reg:DI %r4)
                    (reg:DI %r3)))
      (cinsn 8 (set (reg:CC %cr0)
                    (compare:CC (reg:DI %r3)
                        (const_int 0))))
      ;; Extra insn to avoid the above being deleted by DCE.
      (cinsn 18 (use (reg:DI %r4)))
      (cinsn 19 (use (reg:CC %cr0)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function "main"
}

/* { dg-final { scan-assembler-times {\mmr\.} 2 } } */
