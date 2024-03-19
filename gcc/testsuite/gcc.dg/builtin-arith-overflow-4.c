/* PR c/90628 */
/* { dg-do compile } */
/* { dg-options "-fpermissive" } */

_Atomic int a = 1, b = 2, c = 3;
_Atomic long d = 4, e = 5, f = 6;
_Atomic long long g = 7, h = 8, i = 9;

void
f1 ()
{
  __builtin_add_overflow (a, b, &c);	/* { dg-error "argument 3 in call to function '__builtin_add_overflow' has pointer to '_Atomic' type" } */
}

void
f2 ()
{
  __builtin_sub_overflow (d, e, &f);	/* { dg-error "argument 3 in call to function '__builtin_sub_overflow' has pointer to '_Atomic' type" } */
}

void
f3 ()
{
  __builtin_mul_overflow (g, h, &i);	/* { dg-error "argument 3 in call to function '__builtin_mul_overflow' has pointer to '_Atomic' type" } */
}

void
f4 ()
{
  __builtin_sadd_overflow (a, b, &c);	/* { dg-warning "passing argument 3 of '__builtin_sadd_overflow' from incompatible pointer type" } */
}

void
f5 ()
{
  __builtin_ssubl_overflow (d, e, &f);	/* { dg-warning "passing argument 3 of '__builtin_ssubl_overflow' from incompatible pointer type" } */
}

void
f6 ()
{
  __builtin_smulll_overflow (g, h, &i);	/* { dg-warning "passing argument 3 of '__builtin_smulll_overflow' from incompatible pointer type" } */
}
