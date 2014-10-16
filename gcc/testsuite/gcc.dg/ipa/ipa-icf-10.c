/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf"  } */

int ferda(int x, int y) __attribute__ ((pure));
int funkce(int a, int b) __attribute__ ((pure));

__attribute__ ((noinline))
int ferda(int x, int y)
{
  if (x < y)
    {
      return x;
    }
  else
    return y;
}

__attribute__ ((noinline))
int funkce(int a, int b)
{
  if(a < b)
    return a;
  else
    return b;
}

int main(int argc, char **argv)
{
  return 0;
}

/* { dg-final { scan-ipa-dump "Semantic equality hit:funkce->ferda" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */
/* { dg-final { cleanup-ipa-dump "icf" } } */
