// { dg-options "-std=c++23 -fcontracts -fcontracts-nonattr -fcontract-evaluation-semantic=observe" }
// { dg-additional-options "-O -g" }

// Check that we do not ICE with debug + optimisation.

int foo (const int i)
  pre (i > 3)
{
  return i;
}

int main()
{
  foo (1);
}
