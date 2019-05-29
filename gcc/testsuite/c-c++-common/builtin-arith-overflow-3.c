/* PR c/90628 */
/* { dg-do compile } */

const int a = 1, b = 2, c = 3;
const long d = 4, e = 5, f = 6;
const long long g = 7, h = 8, i = 9;

void
f1 ()
{
  __builtin_add_overflow (a, b, &c);	/* { dg-error "argument 3 in call to function '__builtin_add_overflow' has pointer to 'const' type" } */
}

void
f2 ()
{
  __builtin_sub_overflow (d, e, &f);	/* { dg-error "argument 3 in call to function '__builtin_sub_overflow' has pointer to 'const' type" } */
}

void
f3 ()
{
  __builtin_mul_overflow (g, h, &i);	/* { dg-error "argument 3 in call to function '__builtin_mul_overflow' has pointer to 'const' type" } */
}

void
f4 ()
{
  __builtin_sadd_overflow (a, b, &c);	/* { dg-warning "passing argument 3 of '__builtin_sadd_overflow' discards 'const' qualifier from pointer target type" "" { target c } } */
}	/* { dg-error "invalid conversion from 'const int\\*' to 'int\\*'" "" { target c++ } .-1 } */

void
f5 ()
{
  __builtin_ssubl_overflow (d, e, &f);	/* { dg-warning "passing argument 3 of '__builtin_ssubl_overflow' discards 'const' qualifier from pointer target type" "" { target c } } */
}	/* { dg-error "invalid conversion from 'const long int\\*' to 'long int\\*'" "" { target c++ } .-1 } */

void
f6 ()
{
  __builtin_smulll_overflow (g, h, &i);	/* { dg-warning "passing argument 3 of '__builtin_smulll_overflow' discards 'const' qualifier from pointer target type" "" { target c } } */
}	/* { dg-error "invalid conversion from 'const long long int\\*' to 'long long int\\*'" "" { target c++ } .-1 } */
