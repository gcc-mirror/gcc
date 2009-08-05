// PR rtl-optimization/40924
// { dg-do run }

extern "C" void abort (void);

#define MAY_ALIAS __attribute__((__may_alias__))

typedef struct { float v[2]; } floata;
typedef struct { int v[2]; } inta;

typedef unsigned int uint MAY_ALIAS;
typedef signed int sint MAY_ALIAS;
typedef float flt MAY_ALIAS;

static inline unsigned short
less_than (inta a, inta b)
{
  unsigned short r = 0;
  const uint *p1 = (const uint *) &a;
  const uint *p2 = (const uint *) &b;
  for (int i=0; i < 2; i++)
    if (p1[i] < p2[i]) r |= (1 << i);
  return r;
}

static inline inta
multiply (inta b, inta c)
{
  inta r;
  sint *p3 = (sint *) &c;
  for (int i=0; i < 2; i++)
    r.v[i] = (int) (b.v[i] * p3[i] & 0xFFFFFFFF);
  return r;
}

static inline floata
gather (inta indexes, const void *baseAddr)
{
  floata r;

  sint *idx = (sint *) &indexes;
  flt *src = (flt *) baseAddr;
  for (int i=0; i < 2; i++)
    r.v[i] = *(src + idx[i]);
  return r;
}

static inline inta
add (const inta &b, const inta &c)
{
  inta result;
  sint *r = (sint *) &result;

  for (int i=0; i < 2; i++)
    r[i] = b.v[i] + c.v[i];
  return result;
}

struct uintv
{
  inta data;
  inline uintv () { data.v[0] = 0; data.v[1] = 1; }
  inline uintv (unsigned int a)
  {
    for (int i=0; i < 2; i++)
      *(uint *) &data.v[i] = a;
  }
  inline uintv (inta x) : data (x) {}
  inline uintv operator* (const uintv &x) const
  { return multiply (data, x.data); }
  inline uintv operator+ (const uintv &x) const
  { return uintv (add (data, x.data)); }
  inline unsigned short operator< (const uintv &x) const
  { return less_than (data, x.data); }
};

struct floatv
{
  floata data;
  explicit inline floatv (const uintv &x)
  {
    uint *p2 = (uint *) &x.data;
    for (int i=0; i < 2; i++)
      data.v[i] = p2[i];
  }
  inline floatv (const float *array, const uintv &indexes)
  {
    const uintv &offsets = indexes * uintv (1);
    data = gather (offsets.data, array);
  }
  unsigned short operator== (const floatv &x) const
  {
    unsigned short r = 0;
    for (int i=0; i < 2; i++)
      if (data.v[i] == x.data.v[i]) r |= (1 << i);
    return r;
  }
};

int
main ()
{
  const float array[2] = { 2, 3 };
  for (uintv i; (i < 2) == 3; i = i + 2)
    {
      const floatv ii (i + 2);
      floatv a (array, i);
      if ((a == ii) != 3)
	abort ();
    }
}
