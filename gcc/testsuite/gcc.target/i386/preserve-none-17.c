/* { dg-do compile } */
/* { dg-options "-O2" } */

extern void foo (void) __attribute__ ((preserve_none));

void
foo (void)
{
}
