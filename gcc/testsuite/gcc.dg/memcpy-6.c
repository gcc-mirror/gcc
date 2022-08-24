/* Test to verify that overlapping memcpy with const sizes that are powers
   of two are folded into into the same code as memmove, but that they
   are diagnosed nonetheless.  Whether a call is folded depends on
   the size of the copy, the alignment, and wheteber else the target
   might decide to consider.  The test is only run on a small subset
   of targets where it's known to pass (see PR testsuite/83483).
   { dg-do compile }
   { dg-options "-O0 -Wrestrict -fdump-tree-optimized" }
   { dg-skip-if "skip non-x86 targets" { ! { i?86-*-* x86_64-*-* } } }
   { dg-additional-options "-msse" { target i?86-*-* x86_64-*-* } } */

char a[32];

void fold_copy_2 (void)
{
  __builtin_memcpy (a + 1, a, 2);   /* { dg-warning "\\\[-Wrestrict]" } */
}

void fold_copy_4 (void)
{
  __builtin_memcpy (a + 2, a, 4);   /* { dg-warning "\\\[-Wrestrict]" } */
}

void fold_copy_8 (void)
{
  __builtin_memcpy (a + 3, a, 8);   /* { dg-warning "\\\[-Wrestrict]" } */
}

void fold_move_2 (void)
{
  __builtin_memmove (a + 1, a, 2);
}

void fold_move_4 (void)
{
  __builtin_memmove (a + 2, a, 4);
}

void fold_move_8 (void)
{
  __builtin_memmove (a + 3, a, 8);
}

/* { dg-final { scan-tree-dump-not "memcpy" "optimized" } }
   { dg-final { scan-tree-dump-not "memmove" "optimized" } } */
