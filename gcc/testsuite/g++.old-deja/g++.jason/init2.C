// { dg-do run  }
// PRMS Id: 5126

extern int i, j;
static const int foo [] = { i, j };
int i = 5, j = 42;
int main()
{
  return foo[1] != 42;
}
