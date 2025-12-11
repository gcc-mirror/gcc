/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O2 -fpic -fno-plt -mno-explicit-relocs -mtls-dialect=trad -mcmodel=medium" } */
/* { dg-final { check-function-bodies "**" "" } } */

extern void g (void);
void __attribute__ ((noinline))
f (void)
{}

static void __attribute__ ((noipa,noinline))
l (void)
{}

/*
** test:
** 	la.global	(\$r[0-9]+),g
** 	jr	(\$r[0-9]+)
*/
void
test (void)
{
  g ();
}

/*
** test1:
** 	la.global	(\$r[0-9]+),f
** 	jr	(\$r[0-9]+)
*/
void
test1 (void)
{
  f ();
}

/*
** test2:
** 	la.local	(\$r[0-9]+),l
** 	jr	(\$r[0-9]+)
*/
void
test2 (void)
{
  l ();
}

__attribute__ ((tls_model ("global-dynamic"))) __thread int a;

void
test3 (void)
{
  a = 10;
}
/* { dg-final { scan-assembler "test3:.*la\.global\t.*\_\_tls\_get\_addr" { target tls_native } } } */
