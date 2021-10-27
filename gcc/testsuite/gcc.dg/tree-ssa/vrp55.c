/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ethread-details -fdelete-null-pointer-checks" } */

void arf (void);

void
fu (char *p, int x)
{
  if (x)
   *p = 69;
  if (p)
    arf ();
}

/* { dg-final { scan-tree-dump-times "Registering jump thread" 1 "ethread" { target { ! keeps_null_pointer_checks } } } } */
/* { dg-final { scan-tree-dump-times "Registering jump thread" 0 "ethread" { target {   keeps_null_pointer_checks } } } } */

