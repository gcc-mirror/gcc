/* PR c++/97947 */
/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Verify we do not ICE on the test below.  */

void
bug (__vector_pair *src)
{
  volatile __vector_pair dd = *src;
}
