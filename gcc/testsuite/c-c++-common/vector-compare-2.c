/* { dg-do compile } */   
/* { dg-options "-fno-common" { target hppa*-*-hpux* } } */
/* { dg-options "-mabi=altivec" { target { { powerpc*-*-linux* } && ilp32 } } } */
/* { dg-options "-msse2" { target { i?86-*-* x86_64-*-* } } } */
/* Ignore warning on some powerpc-ibm-aix configurations. */
/* { dg-prune-output "non-standard ABI extension" } */

/* Test if C_MAYBE_CONST are folded correctly when 
   creating VEC_COND_EXPR.  */

typedef int vec __attribute__((vector_size(16)));

vec i,j;
extern vec a, b, c;

extern int p, q, z;
extern vec foo (int);

vec 
foo (int x)
{
  return  foo (p ? q :z) > a;
}

vec 
bar (int x)
{
  return  b > foo (p ? q :z);
}


