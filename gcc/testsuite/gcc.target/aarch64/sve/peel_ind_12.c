/* Peeling for alignment with masking together with versioning in VLA modes.  */
/* { dg-do compile } */
/* { dg-options "-Ofast -msve-vector-bits=scalable --param aarch64-autovec-preference=sve-only -fdump-tree-vect-details" } */

#define START 5
#define END 509

int __attribute__((noipa))
foo (int *restrict a, int * restrict b) {
  for (signed int i = START; i < END; ++i) {
    if (a[i] != b[i])
      return i;
  }
  return -1;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump "Both peeling and versioning will be applied" "vect" } } */
/* { dg-final { scan-assembler {\tnot\tp[0-7]\.b, p[0-7]/z, p.*\n} } } */
/* { dg-final { scan-assembler {\teor\t.*\n} } } */
