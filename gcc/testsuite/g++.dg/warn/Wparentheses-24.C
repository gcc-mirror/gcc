// { dg-do compile }
// { dg-options "-Wparentheses" }

extern int foo (int);

bool a, b, c;

bool
bar ()
{
  c = a = b;
  foo (0);
  return a = b;
}
