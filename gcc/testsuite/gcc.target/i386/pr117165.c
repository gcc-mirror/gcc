/* PR target/117165 */
/* { dg-do compile } */
/* { dg-options "-msoft-float" } */

void
foo ()
{
  __builtin_ia32_fnstsw ();	/* { dg-error "implicit declaration of function" } */
}

void
bar ()
{
  __builtin_ia32_fnclex ();	/* { dg-error "implicit declaration of function" } */
}

void
baz ()
{
  __builtin_ia32_fnstenv (0);	/* { dg-error "implicit declaration of function" } */
}

void
qux ()
{
  __builtin_ia32_fldenv (0);	/* { dg-error "implicit declaration of function" } */
}
