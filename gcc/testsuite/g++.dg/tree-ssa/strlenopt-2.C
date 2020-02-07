/* PR tree-optimization/92765 - wrong code for strcmp of a union member
   { dg-do run }
   { dg-options "-O2 -Wall" } */

typedef __SIZE_TYPE__ size_t;

inline void* operator new (size_t, void *p)
{
  return p;
}

struct A
{
  char a[2]; char b[2]; char c[2];
  A () { a[0] = 0; b[0] = 0; c[0] = 0; };
  ~A () { }
};

struct B
{
  char d[6];
  B () { d[0] = 0; d[2] = 0; }
  ~B () { }
};

__attribute__((noipa)) void
sink (void *) { }

__attribute__((noipa)) void
copy (char *d, const char *s)
{
  while ((*d++ = *s++));
}

__attribute__((noipa)) void
store_and_compare (void *p)
{
  A *a = new (p) A ();
  sink (&a->b);
  a->~A ();

  B *b = new (p) B ();
  char *q = &b->d[2];
  copy (q, "abc");

  if (__builtin_strcmp (q, "abc"))
    __builtin_abort ();
  b->~B ();
}

int main ()
{
  char a [sizeof (A) > sizeof (B) ? sizeof (A) : sizeof (B)];
  store_and_compare (a);
  return 0;
}
