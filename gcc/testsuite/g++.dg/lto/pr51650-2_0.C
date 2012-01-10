// { dg-lto-do link }
// { dg-lto-options { { -flto -g } } }

typedef struct { } X;
int main ()
{
  typedef X **P;
  P g = 0;
}
