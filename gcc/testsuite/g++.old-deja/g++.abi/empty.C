// Special g++ Options: -w
// Origin: Mark Mitchell <mark@codesourcery.com>

#if defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100

struct S0
{
};

struct S1 : public S0
{
};

struct S2 : public S1
{
  char c;
};

// In S3, the S1 instance is allocated first at offset zero.  The S2
// instance has to be allocated at a subsequent offset; it's first
// part is also an S1.

struct S3 : public S1, public S2
{
};

struct S4 
{
  int i;
};

// In S4, in contrast to S3, S2 is allocated first, and S1 can be
// allocated on top of S4.

struct S5 : public S2, public S1, public S4
{
};

// The T classes are by-hand layouts that should be equivalent to the
// S classes.

struct T3
{
  S1 s1;
  S2 s2;
};

struct T5
{
  S2 s2;
  S4 s4;
};

int main ()
{
  if (sizeof (S3) != sizeof (T3))
    return 1;
  else if (sizeof (S5) != sizeof (T5))
    return 2;
}

#else /* !(defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100) */

int main () 
{
}

#endif /* !(defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100) */
