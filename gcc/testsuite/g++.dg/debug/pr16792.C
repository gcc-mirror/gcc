// { dg-do compile }

struct S { S(); };
int foo (S b, double j) { };

int main ()
{
  int foo (S, double);
  S v;
}
