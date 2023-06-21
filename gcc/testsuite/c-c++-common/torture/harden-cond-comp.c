/* { dg-do compile } */
/* { dg-options "-fharden-conditional-branches -fharden-compares -fdump-tree-hardcbr -fdump-tree-hardcmp -ffat-lto-objects" } */

int f(int i, int j, int k, int l) {
  if (i == 0)
    return (j != 0) + l;
  else
    return (i * j != 0) * k;
}

/* { dg-final { scan-tree-dump-times "Splitting edge" 2 "hardcbr" } } */
/* { dg-final { scan-tree-dump-times "Adding reversed compare" 2 "hardcbr" } } */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 2 "hardcbr" } } */

/* { dg-final { scan-tree-dump-times "Splitting block" 2 "hardcmp" } } */
/* { dg-final { scan-tree-dump-times "Adding reversed compare" 2 "hardcmp" } } */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 4 "hardcmp" } } */

/* Check that the optimization barrier is placed before the original compare.  */
/* { dg-final { scan-tree-dump-times {__asm__[(]"" : "=g" _[0-9]* : "0" i_[0-9]*[(]D[)][)][;][\n][ ]*if [(]i_[0-9]*[(]D[)] == 0[)]} 1 "hardcbr" } } */
/* { dg-final { scan-tree-dump-times {if [(]_[0-9]* != 0[)]} 2 "hardcbr" } } */

/* { dg-final { scan-tree-dump-times {__asm__[(]"" : "=g" _[0-9]* : "0" j_[0-9]*[(]D[)][)][;][\n][ ]*_[0-9]* = j_[0-9]*[(]D[)] != 0;[\n] *_[0-9]* = _[0-9]* == 0} 1 "hardcmp" } } */
/* { dg-final { scan-tree-dump-times {__asm__[(]"" : "=g" _[0-9]* : "0" _[0-9]*[)][;][\n][ ]*_[0-9]* = _[0-9]* != 0;[\n] *_[0-9]* = _[0-9]* == 0} 1 "hardcmp" } } */
