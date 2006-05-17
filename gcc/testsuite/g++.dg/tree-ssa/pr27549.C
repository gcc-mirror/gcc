// PR tree-optimization/27549
// { dg-do compile }
// { dg-options "-O2" }

typedef __SIZE_TYPE__ size_t;

struct E
{
  virtual ~E () {}
  virtual size_t e () const = 0;
  virtual void f (char *x) const = 0;
};

struct F : public E
{
  virtual ~F () {}
  virtual size_t e () const { return 0; }
  virtual void f (char *x) const { *x = '\0'; }
};

struct S
{
  S () { a = new char[32]; b = 32; c = 0; a[0] = 0; }
  void s (const char *x, size_t y) { v (c + y + 1); __builtin_memcpy(a + c, x, y); c += y; a[c] = '\0'; }
  void s (const E *x) { size_t l = x->e(); v (c + l + 1); x->f (a + c); c += l; }
  const char *t () { return a; }
  void v (size_t n)
    {
      if (b >= n) return;

      size_t b2 = b;
      char *a2 = a;

      for (;;)
	{
	  b *= 2;
	  if (b >= n)
	    break;
	}

      a = new char[b];

      if (b2)
	{
	  __builtin_memcpy(a, a2, c);
	  a2[0] = 0;
	  for (size_t i = 1; i < b2; i++)
	    a2[i] = a2[i - 1];
	  delete[] a2;
	}
    }

  ~S ()
    {
      if (b)
	{
	  a[0] = 0;
	  for (size_t i = 1; i < b; i++)
	    a[i] = a[i - 1];
	}
      delete[] a;
    }
  char * a;
  size_t b, c;
};

const char *p;
size_t q;
const F u;

const char *
foo ()
{
  S s;
  s.s (p, q);
  s.s (&u);
  return s.t ();
}
