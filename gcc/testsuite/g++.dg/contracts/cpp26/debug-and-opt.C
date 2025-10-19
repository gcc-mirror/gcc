// Check that we do not ICE with debug + optimisation.
// { dg-do run { target c++23 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=observe -O -g" }


int foo (const int i)
  pre (i > 3)
{
  return i;
}

int main()
{
  foo (1);
}
