/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O2 -fno-pic -fplt -mno-explicit-relocs -mtls-dialect=trad -mcmodel=medium" } */
/* { dg-final { check-function-bodies "**" "" } } */

extern void g (void);
void __attribute__ ((noipa,noinline))
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
** 	la.local	(\$r[0-9]+),f
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
