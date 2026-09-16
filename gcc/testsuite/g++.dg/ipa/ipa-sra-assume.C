/* Test that ipa-sra can  work with .ASSUME outlined bodies.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fipa-sra -fdump-ipa-sra-details -fdump-tree-optimized" } */

struct S
{
  bool x;
  int y;
};

void
split_struct (struct S s)
{
  [[assume (!s.x)]];
  if (s.x)
    __builtin_abort ();
}

void
remove_ptr (bool *x)
{
  [[assume (!*x)]];
  if (*x)
    __builtin_abort ();
}

void
two_values (struct S s)
{
  [[assume (s.y == 3 || s.y == 4)]];
  if (s.y == 5)
    __builtin_abort ();
}

/* { dg-final { scan-ipa-dump-times "Will split parameter 0" 3 "sra" } } */
/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
