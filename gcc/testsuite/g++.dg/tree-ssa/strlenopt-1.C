/* PR tree-optimization/92765 - wrong code for strcmp of a union member
   { dg-do run }
   { dg-options "-O2 -Wall" } */

typedef __SIZE_TYPE__ size_t;

inline void* operator new (size_t, void *p)
{
  return p;
}

struct A { char a2[2]; };
struct B { char a4[4]; };

__attribute__((noipa)) void
sink (void*) { }

__attribute__((noipa)) void
copy (char *d, const char *s)
{
  while ((*d++ = *s++));
}

__attribute__((noipa)) void
store_and_compare (void *p)
{
  A *a = new (p) A;
  sink (a->a2);

  B *b = new (p) B;
  char *q = (char *) b->a4;
  copy (q, "abc");

  if (__builtin_strcmp (q, "abc"))
    __builtin_abort ();
}

int main ()
{
  char a [sizeof (A) > sizeof (B) ? sizeof (A) : sizeof (B)];
  store_and_compare (a);
}
