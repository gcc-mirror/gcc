// { dg-do run { target i?86-*-linux* x86_64-*-linux* i?86-*-freebsd* i?86-*-darwin* } }
// { dg-require-effective-target ilp32 }
// { dg-options "-malign-double" }
// Origin: Alex Samuel <samuel@codesourcery.com>

/* Test the size and alignment of fundamental C types for compliance
   with the IA-64 ABI.  */

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

enum A { a };

int
main ()
{
  if (sizeof  (char)                    !=  1)
    return 1;
  if (alignof (char)                    !=  1)
    return 2;
  if (sizeof  (signed char)             !=  1)
    return 3;
  if (alignof (signed char)             !=  1)
    return 4;
  if (sizeof  (unsigned char)           !=  1)
    return 5;  
  if (alignof (unsigned char)           !=  1)
    return 6;
  if (sizeof  (short)                   !=  2)
    return 7;
  if (alignof (short)                   !=  2)
    return 8;
  if (sizeof  (signed short)            !=  2)
    return 9;
  if (alignof (signed short)            !=  2)
    return 10;
  if (sizeof  (unsigned short)          !=  2)
    return 11;
  if (alignof (unsigned short)          !=  2)
    return 12;
  if (sizeof  (int)                     !=  4)
    return 13;
  if (alignof (int)                     !=  4)
    return 14;
  if (sizeof  (signed int)              !=  4)
    return 15;
  if (alignof (signed int)              !=  4)
    return 16;
  if (sizeof  (unsigned int)            !=  4)
    return 17;
  if (alignof (unsigned int)            !=  4)
    return 18;
  if (sizeof  (enum A)                  !=  4)
    return 19;
  if (alignof (enum A)                  !=  4)
    return 20;
#ifdef HAVE_IA64_TYPES
  if (sizeof  (__int64)                 !=  8)
    return 21;
  if (alignof (__int64)                 !=  8)
    return 22;
  if (sizeof  (signed __int64)          !=  8)
    return 23;
  if (alignof (signed ___int64)         !=  8)
    return 24;
  if (sizeof  (unsigned __int64)        !=  8)
    return 25;
  if (alignof (unsigned __int64)        !=  8)
    return 26;
  if (sizeof  (__int128)                != 16)
    return 27;
  if (alignof (__int128)                != 16)
    return 28;
  if (sizeof  (signed __int128)         != 16)
    return 29;
  if (alignof (signed ___int128)        != 16)
    return 30;
  if (sizeof  (unsigned __int128)       != 16)
    return 31;
  if (alignof (unsigned ___int128)      != 16)
    return 32;
#endif  /* HAVE_IA64_TYPES  */
  if (sizeof  (void *)                  !=  4)
    return 33;
  if (alignof (void *)                  !=  4)
    return 34;
  if (sizeof  (void (*) ())             !=  4)
    return 35;
  if (alignof (void (*) ())             !=  4)
    return 36;
  if (sizeof  (float)                   !=  4)
    return 37;
  if (alignof (float)                   !=  4)
    return 38;
  if (sizeof  (double)                  !=  8)
    return 39;
  if (alignof (double)                  !=  8)
    return 40;
#ifdef HAVE_IA64_TYPES
  if (sizeof  (__float80)               != 16)
    return 41;
  if (alignof (__float80)               != 16)
    return 42;
  if (sizeof  (__float128)              != 16)
    return 43;
  if (alignof (__float128)              != 16)
    return 44;
#endif  /* HAVE_IA64_TYPES  */

  return 0;
}
