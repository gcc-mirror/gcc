/* { dg-do compile } */
/* { dg-options "-fgimple" } */

void __GIMPLE
foo()
{
bb1:
bb1:; /* { dg-error "duplicate" } */
}
