/* { dg-do compile } */
/* { dg-options "-O2 -gdwarf -dA" } */
void q(int p);
static void
t(int constant_propagated_par)
{
  int local_var = constant_propagated_par + 1;
  q(local_var);
  q(local_var);
  q(local_var);
  q(local_var);
  q(local_var);
  q(local_var);
  q(local_var);
  q(local_var);
  q(local_var);
  q(local_var);
  q(local_var);
}
main()
{
  t(5);
  t(5);
  t(5);
  t(5);
  t(5);
  t(5);
  t(5);
}
/* { dg-final { scan-assembler "local_var" } } */
