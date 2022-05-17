/* PR c++/84850 - -Wclass-memaccess on a memcpy in a copy assignment
   operator with no nontrivial bases or members
   { dg-do compile }
   { dg-options "-Wclass-memaccess -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;

extern "C" void* memcpy (void*, const void*, size_t);
extern "C" void* memset (void*, int, size_t);

template <int>
struct EmptyClass { };

template <int>
struct TrivialClass
{
  bool a;
  int b;
  void *c;
  double d[2];
  void (*e)();
};

template <int>
struct HasDefault
{
  HasDefault ();
};

/* Verify that raw memory accesses from non-static members of a class with
   an empty base is not diagnosed.  */

struct EmptyWithBase: EmptyClass<0>, EmptyClass<1>, EmptyClass<2>
{
  EmptyWithBase ()
  {
    memset (this, 0, sizeof *this);
  }

  EmptyWithBase (const EmptyWithBase &x)
  {
    memcpy (this, &x, sizeof *this);
  }

  ~EmptyWithBase ()
  {
    memset (this, 0, sizeof *this);
  }

  void operator= (const EmptyWithBase &x)
  {
    memcpy (this, &x, sizeof *this);
  }

  void clear ()
  {
    memset (this, 0, sizeof *this);   // { dg-bogus "\\\[-Wclass-memaccess" }
  }

  void copy (const void *p)
  {
    memcpy (this, p, sizeof *this);   // { dg-bogus "\\\[-Wclass-memaccess" }
  }

  static void bad_clear (EmptyWithBase &x)
  {
    memset (&x, 0, sizeof x);         // { dg-warning "\\\[-Wclass-memaccess" }
  }

  static void bad_copy (EmptyWithBase &x, const void *p)
  {
    memcpy (&x, p, sizeof x);         // { dg-warning "\\\[-Wclass-memaccess" }
  }
};

/* Verify that raw memory accesses from non-static members of a class with
   all trivial members is not diagnosed.  */

struct HasTrivialMembers
{
  bool a;
  int b;
  void *c;
  double d[2];
  void (*e)();

  TrivialClass<1> trivial;

  HasTrivialMembers ()
  {
    memset (this, 0, sizeof *this);
  }

  HasTrivialMembers (const HasTrivialMembers &x)
  {
    memcpy (this, &x, sizeof *this);
  }

  ~HasTrivialMembers ()
  {
    memset (this, 0, sizeof *this);
  }

  void operator= (const HasTrivialMembers &x)
  {
    memcpy (this, &x, sizeof *this);
  }

  void clear ()
  {
    memset (this, 0, sizeof *this);   // { dg-bogus "\\\[-Wclass-memaccess" }
  }

  void copy (const void *p)
  {
    memcpy (this, p, sizeof *this);   // { dg-bogus "\\\[-Wclass-memaccess" }
  }

  static void bad_clear (HasTrivialMembers &x)
  {
    memset (&x, 0, sizeof x);         // { dg-warning "\\\[-Wclass-memaccess" }
  }

  static void bad_copy (HasTrivialMembers &x, const void *p)
  {
    memcpy (&x, p, sizeof x);         // { dg-warning "\\\[-Wclass-memaccess" }
  }
};

/* Verify that raw memory accesses from non-static members of a class with
   a trivial base class and no non-trivial members is not diagnosed.  */

struct HasTrivialBase: TrivialClass<1>
{
  TrivialClass<2> a[2];

  HasTrivialBase ()
  {
    memset (this, 0, sizeof *this);   // { dg-bogus "\\\[-Wclass-memaccess" }
  }

  HasTrivialBase (const HasTrivialBase &x)
  {
    memcpy (this, &x, sizeof *this);  // { dg-bogus "\\\[-Wclass-memaccess" }
  }

  ~HasTrivialBase ()
  {
    memset (this, 0, sizeof *this);   // { dg-bogus "\\\[-Wclass-memaccess" }
  }

  void operator= (const HasTrivialBase &x)
  {
    memcpy (this, &x, sizeof *this);
  }

  void clear ()
  {
    memset (this, 0, sizeof *this);   // { dg-bogus "\\\[-Wclass-memaccess" }
  }

  void copy (void *p)
  {
    memcpy (this, p, sizeof *this);   // { dg-bogus "\\\[-Wclass-memaccess" }
  }
};


struct HasTrivialBases: TrivialClass<1>, TrivialClass<2>
{
  TrivialClass<3> a[2];

  HasTrivialBases ()
  {
    memset (this, 0, sizeof *this);   // { dg-bogus "\\\[-Wclass-memaccess" }
  }

  HasTrivialBases (const HasTrivialBases &x)
  {
    memcpy (this, &x, sizeof *this);  // { dg-bogus "\\\[-Wclass-memaccess" }
  }

  ~HasTrivialBases ()
  {
    memset (this, 0, sizeof *this);   // { dg-bogus "\\\[-Wclass-memaccess" }
  }

  void operator= (const HasTrivialBases &x)
  {
    memcpy (this, &x, sizeof *this);
  }

  void clear ()
  {
    memset (this, 0, sizeof *this);   // { dg-bogus "\\\[-Wclass-memaccess" }
  }

  void copy (void *p)
  {
    memcpy (this, p, sizeof *this);   // { dg-bogus "\\\[-Wclass-memaccess" }
  }
};

struct DerivesFromNontrivialClass: HasDefault<1> { };

/* Verify that raw memory accesses from members of a class with a non-trivial
   base class is diagnosed.  */

struct HasNonTrivialBase: TrivialClass<1>, TrivialClass<2>,
			  DerivesFromNontrivialClass,
			  TrivialClass<3>, TrivialClass<4>
{
  HasNonTrivialBase ()
  {
    memset (this, 0, sizeof *this);   // { dg-warning "\\\[-Wclass-memaccess" }
  }

  HasNonTrivialBase (const HasNonTrivialBase &x)
  {
    memcpy (this, &x, sizeof *this);  // { dg-warning "\\\[-Wclass-memaccess" }
  }

  ~HasNonTrivialBase ()
  {
    memset (this, 0, sizeof *this);   // { dg-warning "\\\[-Wclass-memaccess" }
  }

  HasNonTrivialBase& operator= (const HasNonTrivialBase &x)
  {
    memcpy (this, &x, sizeof *this);  // { dg-warning "\\\[-Wclass-memaccess" }
    return *this;
  }

  void clear ()
  {
    memset (this, 0, sizeof *this);   // { dg-warning "\\\[-Wclass-memaccess" }
  }

  void copy (void *p)
  {
    memcpy (this, p, sizeof *this);   // { dg-warning "\\\[-Wclass-memaccess" }
  }
};

struct DerivesIndidirectlyFromNontrivialClass:
  TrivialClass<1>, TrivialClass<2>,
  DerivesFromNontrivialClass,
  TrivialClass<3>, TrivialClass<4> { };

/* Verify that raw memory accesses from members of a class with a non-trivial
   indirect base class is diagnosed.  */

struct HasIndirectNonTrivialBase: TrivialClass<5>, TrivialClass<6>,
				  TrivialClass<7>, TrivialClass<8>,
				  DerivesIndidirectlyFromNontrivialClass
{
  HasIndirectNonTrivialBase ()
  {
    memset (this, 0, sizeof *this);   // { dg-warning "\\\[-Wclass-memaccess" }
  }

  HasIndirectNonTrivialBase (const HasIndirectNonTrivialBase &x)
  {
    memcpy (this, &x, sizeof *this);  // { dg-warning "\\\[-Wclass-memaccess" }
  }

  ~HasIndirectNonTrivialBase ()
  {
    memset (this, 0, sizeof *this);   // { dg-warning "\\\[-Wclass-memaccess" }
  }

  HasIndirectNonTrivialBase& operator= (const HasIndirectNonTrivialBase &x)
  {
    memcpy (this, &x, sizeof *this);  // { dg-warning "\\\[-Wclass-memaccess" }
    return *this;
  }

  void clear ()
  {
    memset (this, 0, sizeof *this);   // { dg-warning "\\\[-Wclass-memaccess" }
  }

  void copy (void *p)
  {
    memcpy (this, p, sizeof *this);   // { dg-warning "\\\[-Wclass-memaccess" }
  }
};
