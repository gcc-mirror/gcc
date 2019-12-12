/* PR middle-end/88546 - Copy attribute unusable for weakrefs
   { dg-do compile }
   { dg-options "-O1 -Wall -fdump-tree-optimized" }
   { dg-require-weak "" } */

#define ATTR(...)   __attribute__ ((__VA_ARGS__))
#define ASRT(expr)   _Static_assert (expr, #expr)

extern "C" {

  ATTR (leaf, nothrow) int
  fnothrow ()
  {
    return 0;
  }

  static __typeof__ (fnothrow)
    ATTR (weakref ("fnothrow"), copy (fnothrow))
    alias_fnothrow;


  ATTR (leaf) int
  fthrow_none () throw ()
  {
    return 0;
  }

  // Verify that no warning is issued for the alias having less
  // restrictive attributes than the target: nothrow.
  static __typeof (fthrow_none)
    ATTR (weakref ("fthrow_none"), copy (fthrow_none))
    alias_fthrow_none;

  // Same as above but with no definition of the target.
  ATTR (leaf) int
  fthrow_none_nodef () throw ();

  static __typeof (fthrow_none_nodef)
    ATTR (weakref ("fthrow_none_nodef"), copy (fthrow_none_nodef))
    alias_fthrow_none_nodef;

  // And again but without using typeof to make sure the nothrow
  // bit is copied by attribute copy alone.
  static int
  ATTR (weakref ("fthrow_none_nodef"), copy (fthrow_none_nodef))
    alias_fthrow_none_nodef_func ();
}


struct UsrClass
{
  ~UsrClass ();
};

// Verify that the nothrow attribute/bit was copied to the alias and
// that no exception handling code is emitted in any of these calls.

int call_alias_fnothrow ()
{
  UsrClass usr;
  return alias_fnothrow ();
}

int call_alias_fthrow_none ()
{
  UsrClass usr;
  return alias_fthrow_none ();
}

int call_alias_fthrow_none_nodef ()
{
  UsrClass usr;
  return alias_fthrow_none_nodef ();
}

int call_alias_fthrow_none_nodef_func ()
{
  UsrClass usr;
  return alias_fthrow_none_nodef_func ();
}

// { dg-final { scan-tree-dump-not "__builtin_unwind" "optimized" } }
