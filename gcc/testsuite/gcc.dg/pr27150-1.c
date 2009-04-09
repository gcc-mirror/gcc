/* { dg-do link } */
/* { dg-options "-O2" } */
extern int link_error ();
int g(int f)
{
  int a = ((&f)!=0);
  if (!a) link_error ();
  return a;
}

int main()
{
  g(10);
}
