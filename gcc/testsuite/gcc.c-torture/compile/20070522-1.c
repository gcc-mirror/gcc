/* { dg-options "-Wno-psabi" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

/* This was PR 31606, we were trying to use TImode to expand the
   struct copy in SRA but TImode is not support on ia32. */

struct Collision {
   int edge;
   int triangle;
  float _M_instance[3] __attribute__((__aligned__));
};
void  get_collisions(struct Collision a) {
  struct Collision b = a;
}
