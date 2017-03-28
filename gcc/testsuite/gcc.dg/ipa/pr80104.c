/* PR ipa/80104 */
/* { dg-do compile } */
/* { dg-options "-fipa-icf" } */

float
a (_Complex float b)
{
  return *&b;
}

float
c (_Complex float b)
{
  return (&b)[0];
}
