// { dg-do run }
// { dg-require-effective-target size20plus }

// Copyright (C) 2006 Free Software Foundation, Inc.

// Originally from PR 16681, found also in init/array15.C
// This variant of the testcase verifies that we do not create
// a temporary on the stack, which is PR 27620.

int i;

extern "C"
void *memcpy (void *dest, const void *src, __SIZE_TYPE__ n)
{
  char *d = (char *) dest;
  const char *s = (const char *) src;
  while (n--)
    d[n] = s[n];
  ++i;
  return dest;
}

struct foo {
  unsigned char buffer[41112];
  foo() ;
  bool check () const;
};

foo::foo ()
  : buffer()
{}

bool foo::check () const
{
  for (unsigned ix = sizeof (buffer); ix--;)
    if (buffer[ix])
      return false;
  return true;
}

void *operator new (__SIZE_TYPE__ size, void *p)
{
  return p;
}

char heap[50000];

int main ()
{
  for (unsigned ix = sizeof (heap); ix--;)
    heap[ix] = ix;

  i = 0;
  foo *f = new (heap) foo ();

  if (i != 0)
    return 1;
  if (!f->check ())
    return 1;
  return 0;
}

  
