/* { dg-options "-O2 -fgraphite-identity -flto" { target lto } } */

typedef long ll;
void foo (int n, ll *p)
{
  while (n--)
    *p += *p;
}

typedef long long lll;
void bar (int n, lll *p)
{
  while (n--)
    *p += *p;
}

int main() { return 0; }
