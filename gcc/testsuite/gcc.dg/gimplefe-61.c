/* { dg-do compile } */
/* { dg-options "-fgimple" } */

volatile int i;

void __GIMPLE foo ()
{
  i ={v} 1;
}
