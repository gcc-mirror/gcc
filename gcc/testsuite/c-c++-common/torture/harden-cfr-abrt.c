/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fdump-tree-hardcfr -ffat-lto-objects" } */

/* Check the noreturn handling of a builtin call.  */

int f(int i) {
  if (!i)
    __builtin_abort ();
  return i;
}

int g() {
  __builtin_abort ();
}

/* Out-of-line checking, before both builtin_abort and return in f.  */
/* { dg-final { scan-tree-dump-times "__hardcfr_check" 2 "hardcfr" } } */
/* Inline checking before builtin_return in g.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 1 "hardcfr" } } */
