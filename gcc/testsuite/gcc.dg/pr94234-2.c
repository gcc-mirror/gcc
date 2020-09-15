/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1" } */ 

int use_fn (int a);

int foo (int n)
{
  int b1 = 8 * (n + 1);
  int b2 = 8 * n;

  use_fn (b1 ^ b2);

  return b1 - b2;
}

unsigned goo (unsigned m_param, unsigned n_param)
{
  unsigned b1 = m_param * (n_param + 2);
  unsigned b2 = m_param * (n_param + 1);

  use_fn (b1 ^ b2);

  return b1 - b2;
}

unsigned hoo (unsigned k_param)
{
  unsigned b1 = k_param * 28;
  unsigned b2 = k_param * 15;
  unsigned b3 = k_param * 12;

  use_fn (b1 ^ b2 ^ b3);

  return (b1 - b2) - b3;
}

/* { dg-final { scan-tree-dump-times "return 8;" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "return m_param" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-not "return k_param" "forwprop1" } } */
