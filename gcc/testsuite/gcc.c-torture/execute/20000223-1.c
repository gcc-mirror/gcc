/* Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Nathan Sidwell 23 Feb 2000 <nathan@codesourcery.com> */

/* __alignof__ should never return a non-power of 2
   eg, sizeof(long double) might be 12, but that means it must be alignable
   on a 4 byte boundary. */

void check (char const *type, int align)
{
  if ((align & -align) != align)
    {
      abort ();
    }
}

#define QUOTE_(s) #s
#define QUOTE(s) QUOTE_(s)

#define check(t) check(QUOTE(t), __alignof__(t))

// This struct should have an alignment of the lcm of all the types. If one of
// the base alignments is not a power of two, then A cannot be power of two
// aligned.
struct A
{
  char c;
  signed short ss;
  unsigned short us;
  signed int si;
  unsigned int ui;
  signed long sl;
  unsigned long ul;
  signed long long sll;
  unsigned long long ull;
  float f;
  double d;
  long double ld;
  void *dp;
  void (*fp)();
};

int main ()
{
  check (void);
  check (char);
  check (signed short);
  check (unsigned short);
  check (signed int);
  check (unsigned int);
  check (signed long);
  check (unsigned long);
  check (signed long long);
  check (unsigned long long);
  check (float);
  check (double);
  check (long double);
  check (void *);
  check (void (*)());
  check (struct A);
  return 0;
}
