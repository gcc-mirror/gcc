/* { dg-do compile } */
/* { dg-options "" } */

void *foo(void)
{
  return (void *)0 - 1;
}
