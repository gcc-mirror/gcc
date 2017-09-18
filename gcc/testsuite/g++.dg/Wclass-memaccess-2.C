// PR c++/81169 - -Wclass-memaccess illegitimate warning related to volatile
// { dg-do compile }
// { dg-options "-Wclass-memaccess" }

struct S { int x; };

void cast_const (const S *p)
{
  __builtin_memset (const_cast<S*>(p), 0, sizeof *p);
}

void cast_volatile (volatile S *p)
{
  __builtin_memset (const_cast<S*>(p), 0, sizeof *p);
}

void cast_const_volatile (const volatile S *p)
{
  __builtin_memset (const_cast<S*>(p), 0, sizeof *p);
}

void c_cast_const_volatile (const volatile S *p)
{
  __builtin_memset ((S*)p, 0, sizeof *p);
}

// A C cast to void* suppresses the warning because it casts away
// the qualifiers from the otherwise trivial pointed-to type..
void c_void_cast_const_volatile (const volatile S *p)
{
  __builtin_memset ((void*)p, 0, sizeof *p);
}

// Also verify that casting to char* suppresses the warning for
// non-trivial types.

struct NonTrivial
{
  NonTrivial ();
  NonTrivial (const NonTrivial&);
  NonTrivial& operator= (const NonTrivial&);
  ~NonTrivial ();
};

void cast_void (NonTrivial *p)
{
  __builtin_memset (reinterpret_cast<char*>(p), 0, sizeof *p);
}

// A C cast to a character (or any trivial) type suppresses the warning.
void c_cast_uchar (NonTrivial *p)
{
  __builtin_memset ((unsigned char*)p, 0, sizeof *p);
}

// A cast to void* does not suppress the warning.  That is (or can be)
// considered a feature.
void c_cast_void (NonTrivial *p)
{
  __builtin_memset ((void*)p, 0, sizeof *p);   // { dg-warning "\\\[-Wclass-memaccess]" }
}
