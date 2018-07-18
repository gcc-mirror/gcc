/* PR c/53037.  */
/* { dg-do compile } */
/* { dg-options "-O0 -Wall" } */

struct __attribute__ ((aligned (8))) S8 { char a[8]; };
struct __attribute__ ((packed)) S1 { /* { dg-warning "alignment 1 of 'S1' is less than 8" } */
  struct S8 s8;
};

struct __attribute__ ((packed, aligned (8))) S2 {
  struct S8 s8;
};

struct __attribute__ ((packed, aligned (8))) S3 {
  int i1;
  struct S8 s8; /* { dg-warning "'S3::s8' offset 4 in 'S3' isn't aligned to 8" } */
};

struct __attribute__ ((packed, aligned (8))) S4 {
  int i1;
  int i2;
  struct S8 s8;
};

struct __attribute__ ((packed)) S5 {
   long long ll;
};

union __attribute__ ((packed)) U1 { /* { dg-warning "alignment 1 of 'U1' is less than 8" } */
  int i1;
  struct S8 s8;
};

union __attribute__ ((packed, aligned (8))) U2 {
  int i1;
  struct S8 s8;
};
