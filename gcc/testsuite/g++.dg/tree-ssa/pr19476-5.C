/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-dom2 -fdelete-null-pointer-checks" } */
/* { dg-skip-if "" keeps_null_pointer_checks } */

// See pr19476-1.C for a version that includes <new>.

int g(){
  return 42 + (0 == new int[50]);
}

/* { dg-final { scan-tree-dump     "return 42" "dom2" } } */
