// { dg-do compile }

struct S { S(); };
int foo (S b, double j) { return 0; };

int main ()
{
  int foo (S, double);
  S v;
}
