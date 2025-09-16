/* { dg-do compile } */
/* { dg-options "-fgimple" } */
/* PR c/121421 */
/* Gimple functions cannot be nested functions. */


void main(void)
{
    void __GIMPLE b(){} /* { dg-error "cannot be a nested" } */
}
