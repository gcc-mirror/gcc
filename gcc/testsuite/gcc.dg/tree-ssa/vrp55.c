/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1-blocks-vops-details -fdelete-null-pointer-checks" } */

void arf (void);

void
fu (char *p, int x)
{
  if (x)
   *p = 69;
  if (p)
    arf ();
}

/* { dg-final { scan-tree-dump-times "Threaded jump" 1 "vrp1" { target { ! keeps_null_pointer_checks } } } } */
/* { dg-final { scan-tree-dump-times "Threaded jump" 0 "vrp1" { target {   keeps_null_pointer_checks } } } } */

