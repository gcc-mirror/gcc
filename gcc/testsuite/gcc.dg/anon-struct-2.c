/* { dg-options "-std=gnu89" } */
/* In GNU C mode, we recognize the anonymous struct/union extension,
   but not Microsoft extensions.  */

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
  struct { char z; };
  char e;
};
char testE[sizeof(struct E) == 2 * sizeof(struct A) ? 1 : -1];

/* MS extension.  */
typedef struct A typedef_A;
struct F {
  typedef_A;			/* { dg-warning "does not declare anything" } */
  char f;
};
char testF[sizeof(struct F) == sizeof(struct A) ? 1 : -1];

/* Test that __extension__ does the right thing coming _from_ GNU C mode.  */
__extension__ struct G {
  struct { char z; };
  char g;
};
char testG[sizeof(struct G) == 2 * sizeof(struct A) ? 1 : -1];

struct H {
  struct { char z; };
  char h;
};
char testH[sizeof(struct H) == 2 * sizeof(struct A) ? 1 : -1];
