/* { dg-options "-fms-extensions" } */
/* Verify that enabling Microsoft mode doesn't twist C++ as much as
   their corresponding C extensions.  Checked vs
   Microsoft (R) 32-bit C/C++ Optimizing Compiler Version 12.00.8168 for 80x86
 */

struct A { char a; };

struct B {
  struct A;			/* forward decl of B::A.  */
  char b;
};
char testB[sizeof(B) == sizeof(A) ? 1 : -1];

struct C {
  struct D { char d; };		/* decl of C::D.  */
  char c;
};
char testC[sizeof(C) == sizeof(A) ? 1 : -1];
char testD[sizeof(C::D) == sizeof(A) ? 1 : -1];

struct E {
  struct { char z; };
  char e;
};
char testE[sizeof(E) == 2 * sizeof(A) ? 1 : -1];
char testEz[sizeof( ((E *)0)->z )];

typedef struct A typedef_A;
struct F {
  typedef_A;			/* { dg-error "does not declare anything" } */
  char f;
};
char testF[sizeof(F) == sizeof(A) ? 1 : -1];
