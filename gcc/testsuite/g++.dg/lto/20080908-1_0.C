/* { dg-lto-do run }  */
extern "C" { extern void *memcpy (void *, const void *, unsigned); }

inline int
bci (const float &source)
{
 int dest;
 memcpy (&dest, &source, sizeof (dest));
 return dest;
}

inline float
bcf (const int &source)
{
 float dest;
 memcpy (&dest, &source, sizeof (dest));
 return dest;
}

float
Foo ()
{
 const int foo = bci (0.0f);
 int bar = foo;
 const int baz = foo & 1;
 if (!baz && (foo & 2))
   bar = 0;
 return bcf (bar);
}

int main ()
{
  if (Foo () != 0.0)
    return 1;
  return 0;
}
