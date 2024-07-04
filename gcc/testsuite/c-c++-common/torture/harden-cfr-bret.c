/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fno-exceptions -fdump-tree-hardcfr -ffat-lto-objects" } */
/* { dg-require-effective-target untyped_assembly } */

extern int foobar (void);

#if __cplusplus
typedef void (*fnt)(...);
#else
typedef void (*fnt)();
#endif

int i;

int f(void) {
  if (i)
    __builtin_return (__builtin_apply ((fnt)foobar,
				       __builtin_apply_args (), 0));
  return i;
}

int g(void) {
  __builtin_return (__builtin_apply ((fnt)foobar,
				     __builtin_apply_args (), 0));
}

/* Out-of-line checking, before both builtin_return and return in f.  */
/* { dg-final { scan-tree-dump-times "__hardcfr_check" 2 "hardcfr" } } */
/* Inline checking before builtin_return in g.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 1 "hardcfr" } } */
