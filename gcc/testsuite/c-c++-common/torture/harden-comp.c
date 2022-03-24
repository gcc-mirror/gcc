/* { dg-do compile } */
/* { dg-options "-fharden-compares -fdump-tree-hardcmp -ffat-lto-objects" } */

int
f (int i, int j)
{
  return i < j;
}

/* { dg-final { scan-tree-dump-times "Splitting block" 1 "hardcmp" } } */
/* { dg-final { scan-tree-dump-times "Adding reversed compare" 1 "hardcmp" } } */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 1 "hardcmp" } } */
/* { dg-final { scan-tree-dump-times "_\[0-9\]* = i_\[0-9\]*\[(\]D\[)\] < j_\[0-9\]*\[(\]D\[)\];" 1 "hardcmp" } } */
/* { dg-final { scan-tree-dump-times "_\[0-9\]* = _\[0-9\]* >= _\[0-9\]*;" 1 "hardcmp" } } */
