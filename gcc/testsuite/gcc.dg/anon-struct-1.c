/* { dg-options "-std=iso9899:1990" } */
/* In strict ISO C mode, we don't recognize the anonymous struct/union
   extension or any Microsoft extensions.  */

struct A { char a; };

/* MS extension.  */
struct B {
  struct A;			/* { dg-warning "does not declare anything" } */
  char b;
};
char testB[sizeof(struct B) == sizeof(struct A) ? 1 : -1];

/* MS extension.  */
struct C {
  struct D { char d; };		/* { dg-warning "does not declare anything" } */
  char c;
};
char testC[sizeof(struct C) == sizeof(struct A) ? 1 : -1];
char testD[sizeof(struct D) == sizeof(struct A) ? 1 : -1];

/* GNU extension.  */
struct E {
  struct { char z; };		/* { dg-warning "does not declare anything" } */
  char e;
};
char testE[sizeof(struct E) == sizeof(struct A) ? 1 : -1];

/* MS extension.  */
typedef struct A typedef_A;
struct F {
  typedef_A;			/* { dg-warning "does not declare anything" } */
  char f;
};
char testF[sizeof(struct F) == sizeof(struct A) ? 1 : -1];

/* __extension__ enables GNU C mode for the duration of the declaration.  */
__extension__ struct G {
  struct { char z; };
  char g;
};
char testG[sizeof(struct G) == 2 * sizeof(struct A) ? 1 : -1];

struct H {
  __extension__ struct { char z; };
  char h;
};
char testH[sizeof(struct H) == 2 * sizeof(struct A) ? 1 : -1];

/* Make sure __extension__ gets turned back off.  */
struct I {
  struct { char z; };		/* { dg-warning "does not declare anything" } */
  char i;
};
char testI[sizeof(struct I) == sizeof(struct A) ? 1 : -1];

