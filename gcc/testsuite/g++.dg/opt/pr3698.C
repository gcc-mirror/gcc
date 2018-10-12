// PR c++/3698
// { dg-do link }
// { dg-options "-O0" }

struct X {
  int i;
};

inline const int&
OHashKey (const X& x)
{
  return x.i;
}

int
main ()
{
 extern const int& OHashKey (const X& x);
 X x;
 return OHashKey (x);
}
