// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflection type traits [meta.reflection.traits], reference, sign,
// array and pointer modifications.

#include <meta>
using namespace std::meta;

struct C { };
enum E1 : int { E10, E11 };
enum E2 : unsigned char { E20, E21 };
enum E3 : short int { E30, E31 };
enum E4 : long long { E40, E41 };

static_assert (remove_reference (^^int) == ^^int);
static_assert (remove_reference (^^const int *) == ^^const int *);
static_assert (remove_reference (^^unsigned long &) == ^^unsigned long);
static_assert (remove_reference (^^const short &) == ^^const short);
static_assert (remove_reference (^^long long &&) == ^^long long);
static_assert (remove_reference (^^volatile char &&) == ^^volatile char);
static_assert (remove_reference (^^int *&) == ^^int *);
static_assert (remove_reference (^^C &) == ^^C);
static_assert (remove_reference (^^C &&) == ^^C);
static_assert (remove_reference (^^C) == ^^C);
static_assert (remove_reference (^^int (&) (int)) == ^^int (int));
static_assert (remove_reference (^^int (&&) (int)) == ^^int (int));
static_assert (remove_reference (^^int (int)) == ^^int (int));
using T1 = int;
static_assert (remove_reference (^^T1) == dealias (^^T1));

static_assert (add_lvalue_reference (^^int) == ^^int &);
static_assert (add_lvalue_reference (^^int &) == ^^int &);
static_assert (add_lvalue_reference (^^const int) == ^^const int &);
static_assert (add_lvalue_reference (^^int *) == ^^int *&);
static_assert (add_lvalue_reference (^^C &) == ^^C &);
static_assert (add_lvalue_reference (^^C) == ^^C &);
static_assert (add_lvalue_reference (^^int (int)) == ^^int (&) (int));
static_assert (add_lvalue_reference (^^int &&) == ^^int &);
static_assert (add_lvalue_reference (^^C &&) == ^^C &);
static_assert (add_lvalue_reference (^^void) == ^^void);
static_assert (add_lvalue_reference (^^const void) == ^^const void);
static_assert (add_lvalue_reference (^^bool (int) const) == ^^bool (int) const);
static_assert (add_lvalue_reference (^^bool (int) &) == ^^bool (int) &);
static_assert (add_lvalue_reference (^^bool (int) const &&) == ^^bool (int) const &&);
static_assert (add_lvalue_reference (^^bool (int)) == ^^bool (&) (int));
using T2 = int &;
static_assert (add_lvalue_reference (^^T2) == dealias (^^T2));

static_assert (add_rvalue_reference (^^int) == ^^int &&);
static_assert (add_rvalue_reference (^^int &&) == ^^int &&);
static_assert (add_rvalue_reference (^^int &) == ^^int &);
static_assert (add_rvalue_reference (^^const int) == ^^const int &&);
static_assert (add_rvalue_reference (^^int *) == ^^int *&&);
static_assert (add_rvalue_reference (^^C &&) == ^^C &&);
static_assert (add_rvalue_reference (^^C) == ^^C &&);
static_assert (add_rvalue_reference (^^int (int)) == ^^int (&&) (int));
static_assert (add_rvalue_reference (^^void) == ^^void);
static_assert (add_rvalue_reference (^^const void) == ^^const void);
static_assert (add_rvalue_reference (^^bool (int) const) == ^^bool (int) const);
static_assert (add_rvalue_reference (^^bool (int) &) == ^^bool (int) &);
static_assert (add_rvalue_reference (^^bool (int) const &&) == ^^bool (int) const &&);
static_assert (add_rvalue_reference (^^bool (int)) == ^^bool (&&) (int));
using T3 = int &&;
static_assert (add_rvalue_reference (^^T3) == dealias (^^T3));

static_assert (make_signed (^^const char) == ^^const signed char);
static_assert (make_signed (^^volatile signed char) == ^^volatile signed char);
static_assert (make_signed (^^unsigned char) == ^^signed char);
static_assert (make_signed (^^const signed short) == ^^const short);
static_assert (make_signed (^^volatile unsigned short) == ^^volatile signed short);
static_assert (make_signed (^^int) == ^^signed int);
static_assert (make_signed (^^const unsigned int) == ^^const int);
static_assert (make_signed (^^volatile long) == ^^volatile signed long);
static_assert (make_signed (^^unsigned long) == ^^long);
static_assert (make_signed (^^const signed long long) == ^^const long long);
static_assert (make_signed (^^volatile long long unsigned) == ^^volatile long long);
#if __SIZEOF_SHORT__ > 1 && __SIZEOF_SHORT__ < __SIZEOF_INT__ \
    && __SIZEOF_INT__ < __SIZEOF_LONG_LONG__
static_assert (make_signed (^^const E1) == ^^const int);
static_assert (make_signed (^^volatile E2) == ^^volatile signed char);
static_assert (make_signed (^^E3) == ^^short);
#if __SIZEOF_LONG__ < __SIZEOF_LONG_LONG__
static_assert (make_signed (^^const volatile E4) == ^^const volatile long long);
#else
static_assert (make_signed (^^const volatile E4) == ^^const volatile long);
#endif
static_assert (make_signed (^^volatile char8_t) == ^^volatile signed char);
static_assert (sizeof (char16_t) != sizeof (short) || make_signed (^^char16_t) == ^^signed short);
static_assert (sizeof (char32_t) != sizeof (int) || make_signed (^^const volatile char32_t) == ^^const volatile int);
static_assert (sizeof (wchar_t) != sizeof (short) || make_signed (^^wchar_t) == ^^short);
static_assert (sizeof (wchar_t) != sizeof (int) || make_signed (^^wchar_t) == ^^signed int);
#endif
static_assert (make_signed (^^T1) == ^^int);

static_assert (make_unsigned (^^const char) == ^^const unsigned char);
static_assert (make_unsigned (^^volatile signed char) == ^^volatile unsigned char);
static_assert (make_unsigned (^^unsigned char) == ^^unsigned char);
static_assert (make_unsigned (^^const signed short) == ^^const unsigned short);
static_assert (make_unsigned (^^volatile unsigned short) == ^^volatile unsigned short);
static_assert (make_unsigned (^^int) == ^^unsigned int);
static_assert (make_unsigned (^^const unsigned int) == ^^const unsigned);
static_assert (make_unsigned (^^volatile long) == ^^volatile unsigned long);
static_assert (make_unsigned (^^unsigned long) == ^^unsigned long);
static_assert (make_unsigned (^^const signed long long) == ^^const unsigned long long);
static_assert (make_unsigned (^^volatile long long unsigned) == ^^volatile unsigned long long);
#if __SIZEOF_SHORT__ > 1 && __SIZEOF_SHORT__ < __SIZEOF_INT__ \
    && __SIZEOF_INT__ < __SIZEOF_LONG_LONG__
static_assert (make_unsigned (^^const E1) == ^^const unsigned int);
static_assert (make_unsigned (^^volatile E2) == ^^volatile unsigned char);
static_assert (make_unsigned (^^E3) == ^^unsigned short);
#if __SIZEOF_LONG__ < __SIZEOF_LONG_LONG__
static_assert (make_unsigned (^^const volatile E4) == ^^const volatile unsigned long long);
#else
static_assert (make_unsigned (^^const volatile E4) == ^^const volatile long unsigned);
#endif
static_assert (make_unsigned (^^volatile char8_t) == ^^volatile unsigned char);
static_assert (sizeof (char16_t) != sizeof (short) || make_unsigned (^^char16_t) == ^^unsigned short);
static_assert (sizeof (char32_t) != sizeof (int) || make_unsigned (^^const volatile char32_t) == ^^const volatile unsigned);
static_assert (sizeof (wchar_t) != sizeof (short) || make_unsigned (^^wchar_t) == ^^unsigned short);
static_assert (sizeof (wchar_t) != sizeof (int) || make_unsigned (^^wchar_t) == ^^unsigned int);
#endif
using T4 = unsigned;
static_assert (make_unsigned (^^T4) == ^^unsigned);

static_assert (remove_extent (^^int) == ^^int);
static_assert (remove_extent (^^int[2]) == ^^int);
static_assert (remove_extent (^^int[2][3]) == ^^int[3]);
static_assert (remove_extent (^^int[][3]) == ^^int[3]);
static_assert (remove_extent (^^const int[2]) == ^^const int);
static_assert (remove_extent (^^C) == ^^C);
static_assert (remove_extent (^^C[2]) == ^^C);
static_assert (remove_extent (^^C[2][3]) == ^^C[3]);
static_assert (remove_extent (^^C[][3]) == ^^C[3]);
static_assert (remove_extent (^^const C[2]) == ^^const C);
static_assert (remove_extent (^^T1) == ^^int);

static_assert (remove_all_extents (^^int) == ^^int);
static_assert (remove_all_extents (^^int[2]) == ^^int);
static_assert (remove_all_extents (^^int[2][3]) == ^^int);
static_assert (remove_all_extents (^^int[][3]) == ^^int);
static_assert (remove_all_extents (^^const int[2][3]) == ^^const int);
static_assert (remove_all_extents (^^C) == ^^C);
static_assert (remove_all_extents (^^C[2]) == ^^C);
static_assert (remove_all_extents (^^C[2][3]) == ^^C);
static_assert (remove_all_extents (^^C[][3]) == ^^C);
static_assert (remove_all_extents (^^const C[2][3]) == ^^const C);
static_assert (remove_all_extents (^^T1) == ^^int);

static_assert (remove_pointer (^^int *) == ^^int);
static_assert (remove_pointer (^^int) == ^^int);
static_assert (remove_pointer (^^const int *) == ^^const int);
static_assert (remove_pointer (^^int **) == ^^int *);
static_assert (remove_pointer (^^C *) == ^^C);
static_assert (remove_pointer (^^C) == ^^C);
static_assert (remove_pointer (^^T1) == ^^int);

static_assert (add_pointer (^^int) == ^^int *);
static_assert (add_pointer (^^int *) == ^^int **);
static_assert (add_pointer (^^const int) == ^^const int *);
static_assert (add_pointer (^^int &) == ^^int *);
static_assert (add_pointer (^^C *) == ^^C **);
static_assert (add_pointer (^^C) == ^^C *);
static_assert (add_pointer (^^void) == ^^void *);
static_assert (add_pointer (^^const void) == ^^const void *);
static_assert (add_pointer (^^volatile void) == ^^volatile void *);
static_assert (add_pointer (^^const volatile void) == ^^const volatile void *);

consteval
{
  try
    {
      make_signed (^^bool);
      throw 1;
    }
  catch (std::meta::exception &)
    {
    }
  try
    {
      make_unsigned (^^bool);
      throw 1;
    }
  catch (std::meta::exception &)
    {
    }
  try
    {
      make_signed (^^float);
      throw 1;
    }
  catch (std::meta::exception &)
    {
    }
  try
    {
      make_unsigned (^^double);
      throw 1;
    }
  catch (std::meta::exception &)
    {
    }
  try
    {
      make_signed (^^C);
      throw 1;
    }
  catch (std::meta::exception &)
    {
    }
  try
    {
      make_unsigned (^^C);
      throw 1;
    }
  catch (std::meta::exception &)
    {
    }
}
