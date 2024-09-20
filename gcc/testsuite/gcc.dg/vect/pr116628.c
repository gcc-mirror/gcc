/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target vect_masked_store } */
/* { dg-additional-options "-Ofast -mcpu=neoverse-v2" { target aarch64-*-* } } */

typedef float c;
c a[2000], b[0];
void d() {
  for (int e = 0; e < 2000; e++)
    if (b[e])
      a[e] = b[e];
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
