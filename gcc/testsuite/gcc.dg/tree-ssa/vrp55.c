/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1-blocks-vops-details" } */

fu (char *p, int x)
{
  if (x)
   *p = 69;
  if (p)
    arf ();
}

/* { dg-final { scan-tree-dump-times "Threaded jump" 1 "vrp1" { target { ! keeps_null_pointer_checks } } } } */
/* { dg-final { scan-tree-dump-times "Threaded jump" 0 "vrp1" { target {   keeps_null_pointer_checks } } } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */

