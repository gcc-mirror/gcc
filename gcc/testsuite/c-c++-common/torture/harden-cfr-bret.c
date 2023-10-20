/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fdump-tree-hardcfr -ffat-lto-objects" } */

int f(int i) {
  if (i)
    __builtin_return (&i);
  return i;
}

int g(int i) {
  __builtin_return (&i);
}

/* Out-of-line checking, before both builtin_return and return in f.  */
/* { dg-final { scan-tree-dump-times "__hardcfr_check" 2 "hardcfr" } } */
/* Inline checking before builtin_return in g.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 1 "hardcfr" } } */
