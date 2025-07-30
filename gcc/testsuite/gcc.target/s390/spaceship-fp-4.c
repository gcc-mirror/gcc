/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mzarch -march=z13 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times {\.SPACESHIP \([^,]+, [^,]+, 0\)} 3 optimized } } */
/* { dg-final { scan-assembler-times {\tk[edx]br\t} 3 } } */
/* { dg-final { scan-assembler-not {\tloc} } } */
/* { dg-final { scan-assembler-not {\tbrc} } } */
/* { dg-final { scan-assembler-not {\tc[edx]br\t} } } */

/* By time of writing this we emit

	kebr	%f0,%f2
	jo	.L2
	je	.L3
	jnh	.L10
	jg	f3@PLT
.L10:
	jg	f2@PLT
.L3:
	jg	f1@PLT
.L2:
	jg	f4@PLT

   which is not optimal.  Instead we could fold the conditional branch with the
   unconditional into something along the lines

	kebr	%f0,%f2
	jo	f4@PLT
	je	f1@PLT
	jnh	f2@PLT
	jg	f3@PLT
*/

void f1 (void);
void f2 (void);
void f3 (void);
void f4 (void);

#define TEST(T, U)		\
  void test_##U (T x, T y)	\
  {				\
    if (x == y)			\
      f1 ();			\
    else if (x < y)		\
      f2 ();			\
    else if (x > y)		\
      f3 ();			\
    else			\
      f4 ();			\
  }

TEST (float, float)
TEST (double, double)
TEST (long double, longdouble)
