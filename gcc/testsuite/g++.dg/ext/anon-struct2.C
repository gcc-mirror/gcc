/* { dg-options "" } */
/* In GNU C++ mode, we recognize the anonymous struct extension,
   but not Microsoft C extensions.  */

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

/* GNU extension.  */
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

/* Test that __extension__ does the right thing coming _from_ GNU C mode.  */
__extension__ struct G {
  struct { char z; };
  char g;
};
char testG[sizeof(G) == 2 * sizeof(A) ? 1 : -1];

struct H {
  struct { char z; };
  char h;
};
char testH[sizeof(H) == 2 * sizeof(A) ? 1 : -1];
