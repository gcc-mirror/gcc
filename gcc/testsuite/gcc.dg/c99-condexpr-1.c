/* Test for types of conditional expressions.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

/* Notes:

   (a) The rules are the same in both C standard versions, but C99 also
   gives us the "restrict" qualifier to play with.

   (b) Within the C standard, the value of a conditional expression can't
   have qualified type - but nor can this be detected.  Because of GCC's
   extended lvalues, the value may in GCC have qualified type if the
   arguments do.  So don't use the following macro with arguments of
   qualified type.

*/

/* Assertion that the type of a conditional expression between E1 and E2
   is T.  Checks the expression both ways round.  */
#define ASSERT_COND_TYPE(E1, E2, T)			\
	do {						\
	  typedef T type;				\
	  typedef type **typepp;			\
	  typedef __typeof(0 ? (E1) : (E2)) ctype;	\
	  typedef __typeof(0 ? (E2) : (E1)) ctype2;	\
	  typedef ctype **ctypepp;			\
	  typedef ctype2 **ctype2pp;			\
	  typepp x = 0;					\
	  ctypepp y = 0;				\
	  ctype2pp z = 0;				\
	  x = y;					\
	  x = z;					\
	} while (0)

void
foo (void)
{
  const void *c_vp;
  void *vp;
  const int *c_ip;
  volatile int *v_ip;
  int *ip;
  const char *c_cp;
  int *restrict *r_ipp;
  typedef void (*fpt)(void);
  fpt fp;
  signed char sc;
  struct s { int p; } st;
  union u { int p; } un;
  /* Arithmetic type.  */
  ASSERT_COND_TYPE (sc, sc, int);
  /* Structure and union.  */
  ASSERT_COND_TYPE (st, st, struct s);
  ASSERT_COND_TYPE (un, un, union u);
  /* Void.  */
  ASSERT_COND_TYPE ((void)0, (void)1, void);
  /* Pointers: examples from 6.5.15 paragraph 8.  */
  ASSERT_COND_TYPE (c_vp, c_ip, const void *);
  ASSERT_COND_TYPE (v_ip, 0, volatile int *);
  ASSERT_COND_TYPE (c_ip, v_ip, const volatile int *);
  ASSERT_COND_TYPE (vp, c_cp, const void *);
  ASSERT_COND_TYPE (ip, c_ip, const int *);
  ASSERT_COND_TYPE (vp, ip, void *);
  /* Null pointer constants.  */
  ASSERT_COND_TYPE (v_ip, (void *)0, volatile int *);
  ASSERT_COND_TYPE (r_ipp, (void *)0, int *restrict *);
  ASSERT_COND_TYPE (fp, 0, fpt);
  ASSERT_COND_TYPE (fp, (void *)0, fpt);
}
