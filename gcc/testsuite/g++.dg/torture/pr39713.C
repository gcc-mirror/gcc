/* { dg-do compile } */

template <typename To, typename From>
static inline To
bitwise_cast (From from)
{
  union
    {
      From f;
      To t;
    } u;
  u.f = from;
  return u.t;
}

extern void foo (unsigned char *);

double
bar ()
{
  unsigned char b[sizeof (unsigned long long)];
  foo (b);
  return bitwise_cast<double> (*(unsigned long long *) b);
}

