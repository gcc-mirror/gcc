/* Peeling for alignment with masking in VLA modes.  */
/* { dg-do compile } */
/* { dg-options "-Ofast -msve-vector-bits=scalable --param aarch64-autovec-preference=sve-only -fdump-tree-vect-details" } */

#define START 3
#define END 510

int __attribute__((noipa))
foo (int *a) {
  for (signed int i = START; i < END; ++i) {
    if (a[i] != 0)
      return i;
  }
  return -1;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump "Alignment of access forced using peeling" "vect" } } */
/* { dg-final { scan-assembler {\tnot\tp[0-7]\.b, p[0-7]/z, p.*\n} } } */
