// Skip if not target: i?86-*-linux* i?86-*-freebsd*
// Special g++ Options: -malign-double
// Origin: Alex Samuel <samuel@codesourcery.com>

/* Test the data layout of C aggregates by checking aggregate size and
   alignment and field offsets for compliance with the IA-64 ABI.  */

template<typename T>
inline unsigned
alignmentof ()
{
  struct S
  {
    char start_;
    T object_;
  };

  return (unsigned) & ((S *) 0)->object_;
}

/* Computes the alignment, in bytes, of TYPE.  */

#define alignof(type) (alignmentof<type> ())

/* Computes the offset of FIELD in AGGREGATE.  */

#define offsetof(aggregate, field) \
  ((unsigned) (& ((aggregate*) 0)->field))


/* Structs S1, S2, S3, S4, and union U5 are taken from Intel, "IA-64
   Software Conventions and Runtime Architecture Guide", version of
   August 1999.  */

struct S1
{
  char c;
};

struct S2
{
  char c;
  char d;
  short s;
  int n;
};

struct S3
{
  char c;
  short s;
};

struct S4
{
  char c;
  double d;
  short s;
};

union U5
{
  char c;
  short s;
  int j;
};



int
main ()
{
  if (sizeof (struct S1) 		!=  1)
    return 1;
  if (alignof (struct S1)		!=  1)
    return 2;
  if (offsetof (struct S1, c)		!=  0)
    return 3;
  
  if (sizeof (struct S2)		!=  8)
    return 4;
  if (alignof (struct S2)       	!=  4)
    return 5;
  if (offsetof (struct S2, c)		!=  0)
    return 6;
  if (offsetof (struct S2, d)		!=  1)
    return 7;
  if (offsetof (struct S2, s)		!=  2)
    return 8;
  if (offsetof (struct S2, n)		!=  4)
    return 9;
  
  if (sizeof (struct S3)		!=  4)
    return 10;
  if (alignof (struct S3)		!=  2)
    return 11;
  if (offsetof (struct S3, c)		!=  0)
    return 12;
  if (offsetof (struct S3, s)		!=  2)
    return 13;
  
  if (sizeof (struct S4)		!= 24)
    return 14;
  if (alignof (struct S4)		!=  8)
    return 15;
  if (offsetof (struct S4, c)		!=  0)
    return 16;
  if (offsetof (struct S4, d)		!=  8)
    return 17;
  if (offsetof (struct S4, s)		!= 16)
    return 18;
  
  if (sizeof (union U5)			!=  4)
    return 19;
  if (alignof (union U5)		!=  4)
    return 20;
  if (offsetof (union U5, c)		!=  0)
    return 21;
  if (offsetof (union U5, s)		!=  0)
    return 22;
  if (offsetof (union U5, j)		!=  0)
    return 23;
  
  return 0;
}
