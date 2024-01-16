/* { dg-do run { target { aarch64*-*-* alpha*-*-* arm*-*-* hppa*-*-* powerpc*-*-* s390*-*-* } } } */
/* { dg-options "-O2 -fdump-tree-esra --param sra-max-scalarization-size-Ospeed=32" } */
/* { dg-additional-options "-mcpu=ev4" { target alpha*-*-* } } */
/* { dg-additional-options "-mno-vsx" { target { powerpc*-*-* && ilp32 } } } */

extern void abort (void);
struct foo { long x; };

struct bar { struct foo f[2]; };

struct baz { struct bar b[2]; };

int
main (int argc, char **argv)
{
  struct baz a = { { { { { 4 }, { 5 } } }, { { { 6 }, { 7 } } }  } };
  int tot = 0;
  for (int i = 0; i < 2; i++)
    for (int j = 0; j < 2; j++)
      tot = (tot*256) + a.b[i].f[j].x;
  if (tot == 0x04050607)
    return 0;
  abort ();
}

/* { dg-final { scan-tree-dump-times "Removing load: a = \\\*.?L.?C.?.?.?0;" 1 "esra" { xfail hppa*64*-*-* } } } */
/* { dg-final { scan-tree-dump-times "SR\[.$\]\[0-9_\]+ = \\\*.?L.?C.?.?.?0\\.b\\\[0\\\]\\.f\\\[0\\\]\\.x" 1 "esra" { xfail hppa*64*-*-* } } } */
/* { dg-final { scan-tree-dump-times "SR\[.$\]\[0-9_\]+ = \\\*.?L.?C.?.?.?0\\.b\\\[0\\\]\\.f\\\[1\\\]\\.x" 1 "esra" { xfail hppa*64*-*-* } } } */
/* { dg-final { scan-tree-dump-times "SR\[.$\]\[0-9_\]+ = \\\*.?L.?C.?.?.?0\\.b\\\[1\\\]\\.f\\\[0\\\]\\.x" 1 "esra" { xfail hppa*64*-*-* } } } */
/* { dg-final { scan-tree-dump-times "SR\[.$\]\[0-9_\]+ = \\\*.?L.?C.?.?.?0\\.b\\\[1\\\]\\.f\\\[1\\\]\\.x" 1 "esra" { xfail hppa*64*-*-* } } } */
