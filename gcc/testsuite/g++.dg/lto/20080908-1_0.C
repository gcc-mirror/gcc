/* { dg-lto-do run }  */
extern "C" { extern void *memcpy (void *, const void *, __SIZE_TYPE__); }

typedef int int32_t __attribute__((mode (__SI__)));

inline int32_t
bci (const float &source)
{
 int32_t dest;
 memcpy (&dest, &source, sizeof (dest));
 return dest;
}

inline float
bcf (const int32_t &source)
{
 float dest;
 memcpy (&dest, &source, sizeof (dest));
 return dest;
}

float
Foo ()
{
 const int32_t foo = bci (0.0f);
 int32_t bar = foo;
 const int32_t baz = foo & 1;
 if (!baz && (foo & 2))
   bar = 0;
 return bcf (bar);
}

int main ()
{
  if (sizeof (int32_t) == sizeof (float))
    {
      if (Foo () != 0.0)
	return 1;
    }
  return 0;
}
