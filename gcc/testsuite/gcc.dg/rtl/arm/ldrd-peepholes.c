/* { dg-do compile { target arm*-*-* } } */
/* { dg-require-effective-target arm_arm_ok } */
/* { dg-require-effective-target arm_ldrd_strd_ok } */
/* { dg-skip-if "Ensure only targetting arm with TARGET_LDRD" { *-*-* } { "-mthumb" } { "" } } */
/* { dg-options "-O3 -marm -fdump-rtl-peephole2" } */

/*
   Test file contains testcases that are there to check.
      1) Each peephole generates the expected patterns.
      2) These patterns match the expected define_insns and generate ldrd/strd.
      2) Memory alias information is not lost in the peephole transformation.

   I don't check the peephole pass on most of the functions here but just check
   the correct assembly is output.  The ldrd/strd peepholes only generate a
   different pattern to the ldm/stm peepholes in some specific cases, and those
   are checked.

   The exceptions are tested by the crafted testcases at the end of this file
   that are named in the pattern foo_x[[:digit:]].

   The first testcase (foo_mem_11) demonstrates bug 88714 is fixed by checking
   that both alias sets in the RTL are preserved.

   All other testcases are only checked to see that they generate a LDRD or
   STRD instruction accordingly.
 */


/* Example of bugzilla 88714 -- memory aliasing info needs to be retained.  */
int __RTL (startwith ("peephole2")) foo_mem_11 (int *a, int *b)
{
(function "foo_mem_11"
  (insn-chain
    (cnote 1 NOTE_INSN_DELETED)
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 101 (set (reg:SI r2)
		      (mem/c:SI (reg:SI r0) [1 S4 A64])) "/home/matmal01/test.c":18)
      (cinsn 102 (set (reg:SI r3)
		      (mem/c:SI (plus:SI (reg:SI r0) (const_int 4)) [2 S4 A32])) "/home/matmal01/test.c":18)
      (cinsn 103 (set (reg:SI r0)
		      (plus:SI (reg:SI r2) (reg:SI r3))) "/home/matmal01/test.c":18)
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:SI r0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "main"
}
/* { dg-final { scan-rtl-dump {Function foo_mem_11.*\(mem/c:SI[^\n]*\[1.*\(mem/c:SI[^\n]*\n[^\n]*\[2.*Function foo11} "peephole2" } } */

/* ldrd plain peephole2.  */
int __RTL (startwith ("peephole2")) foo11 (int *a)
{
(function "foo11"
  (insn-chain
    (cnote 1 NOTE_INSN_DELETED)
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 101 (set (reg:SI r2)
		      (mem/c:SI (reg:SI r0) [0 S4 A64])) "/home/matmal01/test.c":18)
      (cinsn 102 (set (reg:SI r3)
		      (mem/c:SI (plus:SI (reg:SI r0) (const_int 4)) [0 S4 A32])) "/home/matmal01/test.c":18)
      (cinsn 103 (set (reg:SI r0)
		      (plus:SI (reg:SI r2) (reg:SI r3))) "/home/matmal01/test.c":18)
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:SI r0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "main"
}

/* ldrd plain peephole2, which accepts insns initially out of order.  */
int __RTL (startwith ("peephole2")) foo11_alt (int *a)
{
(function "foo11_alt"
  (insn-chain
    (cnote 1 NOTE_INSN_DELETED)
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 102 (set (reg:SI r3)
		      (mem/c:SI (plus:SI (reg:SI r0) (const_int 4)) [0 S4 A32])) "/home/matmal01/test.c":18)
      (cinsn 101 (set (reg:SI r2)
		      (mem/c:SI (reg:SI r0) [0 S4 A64])) "/home/matmal01/test.c":18)
      (cinsn 103 (set (reg:SI r0)
		      (plus:SI (reg:SI r2) (reg:SI r3))) "/home/matmal01/test.c":18)
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:SI r0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "main"
}

/* strd plain peephole2.  */
int __RTL (startwith ("peephole2")) foo12 (int *a)
{
(function "foo12"
  (insn-chain
    (cnote 1 NOTE_INSN_DELETED)
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 101 (set (mem/c:SI (reg:SI r0) [0 S4 A64])
		      (reg:SI r2)) "/home/matmal01/test.c":18)
      (cinsn 102 (set (mem/c:SI (plus:SI (reg:SI r0) (const_int 4)) [0 S4 A32])
		      (reg:SI r3)) "/home/matmal01/test.c":18)
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:SI r0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "main"
}

/* strd of constants -- store interleaved with constant move into register.
   Use same register twice to ensure we use the relevant pattern.  */
int __RTL (startwith ("peephole2")) foo13 (int *a)
{
(function "foo13"
  (insn-chain
    (cnote 1 NOTE_INSN_DELETED)
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 99  (set (reg:SI r2)
		      (const_int 1)) "/home/matmal01/test.c":18)
      (cinsn 101 (set (mem/c:SI (reg:SI r0) [0 S4 A64])
		      (reg:SI r2)) "/home/matmal01/test.c":18)
      (cinsn 100 (set (reg:SI r2)
		      (const_int 0)) "/home/matmal01/test.c":18)
      (cinsn 102 (set (mem/c:SI (plus:SI (reg:SI r0) (const_int 4)) [0 S4 A32])
		      (reg:SI r2)) "/home/matmal01/test.c":18)
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:SI r0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "main"
}

/* strd of constants -- stores after constant moves into registers.
   Use registers out of order, is only way to avoid plain strd while hitting
   this pattern.  */
int __RTL (startwith ("peephole2")) foo14 (int *a)
{
(function "foo14"
  (insn-chain
    (cnote 1 NOTE_INSN_DELETED)
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 99  (set (reg:SI r3)
		      (const_int 1)) "/home/matmal01/test.c":18)
      (cinsn 100 (set (reg:SI r2)
		      (const_int 0)) "/home/matmal01/test.c":18)
      (cinsn 101 (set (mem/c:SI (reg:SI r0) [0 S4 A64])
		      (reg:SI r3)) "/home/matmal01/test.c":18)
      (cinsn 102 (set (mem/c:SI (plus:SI (reg:SI r0) (const_int 4)) [0 S4 A32])
		      (reg:SI r2)) "/home/matmal01/test.c":18)
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:SI r0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "main"
}

/* swap the destination registers of two loads before a commutative operation.
   Here the commutative operation is what the peephole uses to know it can
   swap the register loads around.  */
int __RTL (startwith ("peephole2")) foo15 (int *a)
{
(function "foo15"
  (insn-chain
    (cnote 1 NOTE_INSN_DELETED)
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 100 (set (reg:SI r3)
		      (mem/c:SI (reg:SI r0) [0 S4 A64])) "/home/matmal01/test.c":18)
      (cinsn 101 (set (reg:SI r2)
		      (mem/c:SI (plus:SI (reg:SI r0) (const_int 4)) [0 S4 A32])) "/home/matmal01/test.c":18)
      (cinsn 102 (set (reg:SI r0)
		      (plus:SI (reg:SI r2) (reg:SI r3))) "/home/matmal01/test.c":18
       (expr_list:REG_DEAD (reg:SI r2)
	(expr_list:REG_DEAD (reg:SI r3)
	 (nil))))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:SI r0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "main"
}


/* swap the destination registers of two loads before a commutative operation
   that sets the flags.  */
/*
   NOTE Can't make a testcase for this pattern since there are no insn patterns
   matching the parallel insn in the peephole.

   i.e. until some define_insn is defined matching that insn that peephole can
   never match in real code, and in artificial RTL code any pattern that can
   match it will cause an ICE.

int __RTL (startwith ("peephole2")) foo16 (int *a)
{
(function "foo16"
  (insn-chain
    (cnote 1 NOTE_INSN_DELETED)
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 100 (set (reg:SI r3)
		      (mem/c:SI (reg:SI r0) [0 S4 A64])) "/home/matmal01/test.c":18)
      (cinsn 101 (set (reg:SI r2)
		      (mem/c:SI (plus:SI (reg:SI r0) (const_int 4)) [0 S4 A32])) "/home/matmal01/test.c":18)
      (cinsn 103 (parallel
		  [(set (reg:SI r0)
			(and:SI (reg:SI r3) (reg:SI r2)))
		  (clobber (reg:CC cc))]) "/home/matmal01/test.c":18
       (expr_list:REG_DEAD (reg:SI r2)
	(expr_list:REG_DEAD (reg:SI r3)
	 (nil))))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:SI r0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "main"
}
*/


/* Making patterns that will behave differently between the LDM/STM peepholes
   and LDRD/STRD peepholes.
   gen_operands_ldrd_strd() uses peep2_find_free_register() to find spare
   registers to use.
   peep2_find_free_register() only ever returns registers marked in
   call_used_regs, hence we make sure to leave register 2 and 3 available (as
   they are always on in the defaults marked by CALL_USED_REGISTERS).  */

/* gen_operands_ldrd_strd() purposefully finds an even register to look at
   which would treat the following pattern differently to the stm peepholes.
 */
int __RTL (startwith ("peephole2")) foo_x1 (int *a)
{
(function "foo_x1"
  (insn-chain
    (cnote 1 NOTE_INSN_DELETED)
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 99  (set (reg:SI r5)
		      (const_int 1)) "/home/matmal01/test.c":18)
      (cinsn 101 (set (mem/c:SI (reg:SI r0) [0 S4 A64])
		      (reg:SI r5)) "/home/matmal01/test.c":18)
      (cinsn 100 (set (reg:SI r5)
		      (const_int 0)) "/home/matmal01/test.c":18)
      (cinsn 102 (set (mem/c:SI (plus:SI (reg:SI r0) (const_int 4)) [0 S4 A32])
		      (reg:SI r5)) "/home/matmal01/test.c":18
       (expr_list:REG_DEAD (reg:SI r5)
	 (nil)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:SI r0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "main"
}
/* Ensure we generated a parallel that started with a set from an even register.
   i.e.
   (parallel [
     (set (mem
	  (reg:SI <even>
   */
/* { dg-final { scan-rtl-dump {Function foo_x1.*\(parallel \[\n[^\n]*\(set \(mem[^\n]*\n[^\n]*\(reg:SI (?:[12])?[2468] r(?:[12])?[2468]\).*Function foo_x2} "peephole2" } } */

/* Like above gen_operands_ldrd_strd() would look to start with an even
   register while gen_const_stm_seq() doesn't care.  */
int __RTL (startwith ("peephole2")) foo_x2 (int *a)
{
(function "foo_x2"
  (insn-chain
    (cnote 1 NOTE_INSN_DELETED)
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 99  (set (reg:SI r5)
		      (const_int 1)) "/home/matmal01/test.c":18)
      (cinsn 100 (set (reg:SI r6)
		      (const_int 0)) "/home/matmal01/test.c":18)
      (cinsn 101 (set (mem/c:SI (reg:SI r0) [0 S4 A64])
		      (reg:SI r5)) "/home/matmal01/test.c":18)
      (cinsn 102 (set (mem/c:SI (plus:SI (reg:SI r0) (const_int 4)) [0 S4 A32])
		      (reg:SI r6)) "/home/matmal01/test.c":18)
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:SI r0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "main"
}
/* Ensure generated parallel starts with a set from an even register (as foo_x1).  */
/* { dg-final { scan-rtl-dump {Function foo_x2.*\(parallel \[\n[^\n]*\(set \(mem[^\n]*\n[^\n]*\(reg:SI (?:[12])?[2468] r(?:[12])?[2468]\).*Function foo_x3} "peephole2" } } */

/* When storing multiple values into a register that will be used later, ldrd
   searches for another register to use instead of just giving up.  */
int __RTL (startwith ("peephole2")) foo_x3 (int *a)
{
(function "foo_x3"
  (insn-chain
    (cnote 1 NOTE_INSN_DELETED)
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 99  (set (reg:SI r3)
		      (const_int 1)) "/home/matmal01/test.c":18)
      (cinsn 101 (set (mem/c:SI (reg:SI r0) [0 S4 A64])
		      (reg:SI r3)) "/home/matmal01/test.c":18)
      (cinsn 100 (set (reg:SI r3)
		      (const_int 0)) "/home/matmal01/test.c":18)
      (cinsn 102 (set (mem/c:SI (plus:SI (reg:SI r0) (const_int 4)) [0 S4 A32])
		      (reg:SI r3)) "/home/matmal01/test.c":18)
      (cinsn 103 (set (reg:SI r0)
		      (plus:SI (reg:SI r0) (reg:SI r3))) "/home/matmal01/test.c":18)
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:SI r0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "main"
}
/* Ensure generated parallel starts with a set from an even register (as foo_x1).  */
/* { dg-final { scan-rtl-dump {Function foo_x3.*\(parallel \[\n[^\n]*\(set \(mem[^\n]*\n[^\n]*\(reg:SI (?:[12])?[2468] r(?:[12])?[2468]\).*Function foo_x4} "peephole2" } } */

/* ldrd gen_peephole2_11 but using plus 8 and plus 12 in the offsets.  */
int __RTL (startwith ("peephole2")) foo_x4 (int *a)
{
(function "foo_x4"
  (insn-chain
    (cnote 1 NOTE_INSN_DELETED)
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 101 (set (reg:SI r2)
		      (mem/c:SI (plus:SI (reg:SI r0) (const_int 8)) [0 S4 A64])) "/home/matmal01/test.c":18)
      (cinsn 102 (set (reg:SI r3)
		      (mem/c:SI (plus:SI (reg:SI r0) (const_int 12)) [0 S4 A32])) "/home/matmal01/test.c":18)
      (cinsn 103 (set (reg:SI r0)
		      (plus:SI (reg:SI r2) (reg:SI r3))) "/home/matmal01/test.c":18)
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:SI r0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "main"
}
/* Ensure generated parallel starts with a set from the appropriate offset from
   register 0.
(parallel [
	    (set (reg:SI ...
		(mem/c:SI (plus:SI (reg:SI 0 r0)
			(const_int 8 .*
*/
/* { dg-final { scan-rtl-dump {Function foo_x4.*\(parallel \[\n[^\n]*\(set \(reg:SI[^\n]*\n *\(mem/c:SI \(plus:SI \(reg:SI 0 r0\)\n *\(const_int 8.*Function foo_x5} "peephole2" } } */

/* strd gen_peephole2_12 but using plus 8 and plus 12 in the offsets.  */
int __RTL (startwith ("peephole2")) foo_x5 (int *a)
{
(function "foo12"
  (insn-chain
    (cnote 1 NOTE_INSN_DELETED)
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 101 (set (mem/c:SI (plus:SI (reg:SI r0) (const_int 8)) [0 S4 A64])
		      (reg:SI r2)) "/home/matmal01/test.c":18)
      (cinsn 102 (set (mem/c:SI (plus:SI (reg:SI r0) (const_int 12)) [0 S4 A32])
		      (reg:SI r3)) "/home/matmal01/test.c":18)
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:SI r0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "main"
}
/* Ensure generated parallel starts with a set to the appropriate offset from
   register 0.  */
/* { dg-final { scan-rtl-dump {Function foo_x5.*\(parallel \[\n[^\n]*\(set \(mem/c:SI \(plus:SI \(reg:SI 0 r0\)\n *\(const_int 8.*$} "peephole2" } } */


/* { dg-final { scan-assembler-not "ldm" } } */
/* { dg-final { scan-assembler-not "stm" } } */
/* { dg-final { scan-assembler-times {ldrd\tr[2468], \[r0\]} 4 } } */
/* { dg-final { scan-assembler-times {ldrd\tr[2468], \[r0, #8\]} 1 } } */
/* { dg-final { scan-assembler-times {strd\tr[2468], \[r0\]} 6 } } */
/* { dg-final { scan-assembler-times {strd\tr[2468], \[r0, #8\]} 1 } } */
