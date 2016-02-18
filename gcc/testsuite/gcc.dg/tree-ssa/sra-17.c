/* { dg-do run { target { aarch64*-*-* alpha*-*-* arm*-*-* hppa*-*-* powerpc*-*-* s390*-*-* } } } */
/* { dg-options "-O2 -fdump-tree-esra --param sra-max-scalarization-size-Ospeed=32" } */
/* { dg-additional-options "-mcpu=ev4" { target alpha*-*-* } } */

extern void abort (void);

int
main (int argc, char **argv)
{
  long a[4] = { 7, 19, 11, 255 };
  int tot = 0;
  for (int i = 0; i < 4; i++)
    tot = (tot*256) + a[i];
  if (tot == 0x07130bff)
    return 0;
  abort ();
}

/* { dg-final { scan-tree-dump-times "Removing load: a = \\\*.?L.?C.?.?.?0;" 1 "esra" } } */
/* { dg-final { scan-tree-dump-times "SR\\.\[0-9_\]+ = \\\*.?L.?C.?.?.?0\\\[" 4 "esra" } } */
