// PR c++/20023
// { dg-do compile }
// { dg-options "-O2" }

void f (void);
typedef __SIZE_TYPE__ size_t;
void g (void *a)
{
  size_t b = (size_t) a;
  switch (b)
  {
    case 1:
    f ();
    break;
  }
}
