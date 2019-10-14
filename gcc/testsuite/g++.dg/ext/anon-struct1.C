/* { dg-options "-ansi -pedantic -pedantic-errors" } */
/* In strict ISO C++ mode, we don't recognize the anonymous struct
   extension or any Microsoft C extensions.  */

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
  struct { char z; };		/* { dg-error "10:ISO C\\+\\+ prohibits anonymous structs" } */
  char e;
};

typedef struct A typedef_A;
struct F {
  typedef_A;			/* { dg-error "does not declare anything" } */
  char f;
};
char testF[sizeof(struct F) == sizeof(struct A) ? 1 : -1];

/* __extension__ enables GNU C mode for the duration of the declaration.  */
__extension__ struct G {
  struct { char z; };
  char g;
};
char testG[sizeof(G) == 2 * sizeof(A) ? 1 : -1];

struct H {
  __extension__ struct { char z; };
  char h;
};
char testH[sizeof(H) == 2 * sizeof(A) ? 1 : -1];

/* Make sure __extension__ gets turned back off.  */
struct I {
  struct { char z; };		/* { dg-error "10:ISO C\\+\\+ prohibits anonymous structs" } */
  char i;
};
