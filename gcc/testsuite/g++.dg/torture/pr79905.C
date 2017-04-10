// PR target/79905
// { dg-do compile { target { powerpc*-*-* } } }
// { dg-require-effective-target powerpc_altivec_ok } 
// { dg-options "-maltivec" }

typedef int V4i __attribute__((vector_size(16)));
void a (V4i) {
  vector int b;
  a (b);
}
