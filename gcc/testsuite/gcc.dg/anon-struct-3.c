/* { dg-options "-std=gnu89 -fms-extensions" } */
/* Enabling Microsoft mode makes all of the tests equivalent.  Checked vs
   Microsoft (R) 32-bit C/C++ Optimizing Compiler Version 12.00.8168 for 80x86
 */

struct A { char a; };

struct B {
  struct A;
  char b;
};
char testB[sizeof(struct B) == 2 * sizeof(struct A) ? 1 : -1];

struct C {
  struct D { char d; };
  char c;
};
char testC[sizeof(struct C) == 2 * sizeof(struct A) ? 1 : -1];
char testD[sizeof(struct D) == sizeof(struct A) ? 1 : -1];

struct E {
  struct { char z; };
  char e;
};
char testE[sizeof(struct E) == 2 * sizeof(struct A) ? 1 : -1];

typedef struct A typedef_A;
struct F {
  typedef_A;
  char f;
};
char testF[sizeof(struct F) == 2 * sizeof(struct A) ? 1 : -1];
