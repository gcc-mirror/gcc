/* PR target/70858 */
/* { dg-do compile } */
/* { dg-options "-fpermissive -O2 -mlwp -mbmi -mtbm -mbmi2 -std=gnu11" } */

void
f1 (unsigned long long x, unsigned int y)
{
  __builtin_ia32_lwpval64 (x, y, 1);	/* { dg-warning "implicit declaration of function .__builtin_ia32_lwpval64." "" { target ia32 } } */
}

char
f2 (unsigned long long x, unsigned int y)
{
  return __builtin_ia32_lwpins64 (x, y, 1);	/* { dg-warning "implicit declaration of function .__builtin_ia32_lwpins64." "" { target ia32 } } */
}

unsigned long long
f3 (unsigned long long x, unsigned long long y)
{
  return __builtin_ia32_bextr_u64 (x, y);	/* { dg-warning "implicit declaration of function .__builtin_ia32_bextr_u64." "" { target ia32 } } */
}

unsigned long long
f4 (unsigned long long x)
{
  return __builtin_ia32_bextri_u64 (x, 1);	/* { dg-warning "implicit declaration of function .__builtin_ia32_bextri_u64." "" { target ia32 } } */
}

unsigned long long
f5 (unsigned long long x, unsigned long long y)
{
  return __builtin_ia32_bzhi_di (x, y);	/* { dg-warning "implicit declaration of function .__builtin_ia32_bzhi_di." "" { target ia32 } } */
}

unsigned long long
f6 (unsigned long long x, unsigned long long y)
{
  return __builtin_ia32_pdep_di (x, y);	/* { dg-warning "implicit declaration of function .__builtin_ia32_pdep_di." "" { target ia32 } } */
}

unsigned long long
f7 (unsigned long long x, unsigned long long y)
{
  return __builtin_ia32_pext_di (x, y);	/* { dg-warning "implicit declaration of function .__builtin_ia32_pext_di." "" { target ia32 } } */
}
