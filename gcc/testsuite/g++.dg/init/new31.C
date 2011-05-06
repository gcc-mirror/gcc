// PR c++/48834
// { dg-options -Wuninitialized }
// { dg-do run }

struct S
{
  S ():i (0)
  {
  }
  int i;
};

int
main ()
{
  S *s = new S[2];
  return 0;
}
