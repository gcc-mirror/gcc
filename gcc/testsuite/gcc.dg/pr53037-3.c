/* PR c/53037.  */
/* { dg-do compile } */
/* { dg-options "-O0 -Wall" } */

struct __attribute__ ((aligned (8))) S8 { char a[8]; };
struct __attribute__ ((packed)) S1 {
  struct S8 s8;
}; /* { dg-warning "alignment 1 of 'struct S1' is less than 8" } */

struct __attribute__ ((packed, aligned (8))) S2 {
  struct S8 s8;
};

struct __attribute__ ((packed, aligned (8))) S3 {
  int i1;
  struct S8 s8; /* { dg-warning "'s8' offset 4 in 'struct S3' isn't aligned to 8" } */
};

struct __attribute__ ((packed, aligned (8))) S4 {
  int i1;
  int i2;
  struct S8 s8;
};

struct __attribute__ ((packed)) S5 {
   long long ll;
};

union __attribute__ ((packed)) U1 {
  int i1;
  struct S8 s8;
}; /* { dg-warning "alignment 1 of 'union U1' is less than 8" } */

union __attribute__ ((packed, aligned (8))) U2 {
  int i1;
  struct S8 s8;
};
