// PR c++/80297
// { dg-do compile }

extern const unsigned long int b;
extern const long long int c;

int
foo ()
{
  int a = 809 >> -(b & !c) + b - (long long)(b & !c);
  return a;
}
