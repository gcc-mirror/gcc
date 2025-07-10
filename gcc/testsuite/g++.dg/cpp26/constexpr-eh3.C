// C++26 P3068R5 - Allowing exception throwing in constant-evaluation
// { dg-do compile { target c++26 } }
// { dg-require-effective-target exceptions_enabled }

#include <exception>
#include <new>
#include <typeinfo>

constexpr std::exception a;
constexpr const char *b = a.what ();
constexpr std::bad_exception c;
constexpr const char *d = c.what ();
constexpr std::bad_alloc e;
constexpr const char *f = e.what ();
constexpr std::bad_array_new_length g;
constexpr const char *h = g.what ();
constexpr std::bad_cast i;
constexpr const char *j = i.what ();
constexpr std::bad_typeid k;
constexpr const char *l = k.what ();
constexpr std::exception_ptr m = nullptr;
static_assert (m == nullptr);
constexpr std::exception_ptr n = std::current_exception ();
static_assert (n == nullptr);
constexpr std::exception_ptr o;
static_assert (o == nullptr);
constexpr std::nested_exception p;
static_assert (p.nested_ptr () == nullptr);

struct A { virtual ~A () {} };
struct B { virtual void b (); };
struct C { virtual void c (); };
struct D : private B { virtual void d (); };
struct E { virtual void e (); };
struct F : D, E, private C { virtual void f (); };
struct G { constexpr G () { if (std::uncaught_exceptions () != 0) asm (""); } };
struct H { constexpr H () : h (0) {} constexpr ~H () { if (std::uncaught_exceptions () != h) asm (""); } int h; };
struct I : std::nested_exception { };
struct J { virtual ~J () noexcept = default; };
struct K final { };
struct L : J, std::nested_exception { };
struct M { };
struct N : I, L { };
struct O : private std::nested_exception { };

constexpr int
foo (int x)
{
  if (std::uncaught_exceptions () != 0)
    return -1;
  switch (x)
    {
    case 0:
      try
	{
	  const std::type_info &s = typeid (*(A *) 0);
	  return -1;
	}
      catch (const std::bad_typeid &x)
	{
	  if (std::uncaught_exceptions () != 0)
	    return -1;
	  const char *p = x.what ();
	  return 1;
	}
      catch (...)
	{
	  return -1;
	}
      break;
    case 1:
      try
	{
	  static constexpr F f;
	  D &d = dynamic_cast<D &>((B &) f);
	  return -1;
	}
      catch (std::bad_cast x)
	{
	  const char *p = x.what ();
	  return 2;
	}
      catch (...)
	{
	  return -1;
	}
      break;
    case 2:
      try
	{
	  H h;
	  h.h = 1;
	  if (std::current_exception () != nullptr)
	    return -1;
	  throw G ();
	}
      catch (const G &g)
	{
	  if (std::uncaught_exceptions () != 0)
	    return -1;
	  if (std::current_exception () == nullptr)
	    return -1;
	  return 3;
	}
      catch (...)
	{
	  return -1;
	}
      break;
    case 3:
      try
	{
	  decltype (sizeof 0) x = -64;
	  char (*a)[2] = new char[x][2];
	  delete[] a;
	}
      catch (std::bad_array_new_length x)
	{
	  return 4;
	}
      break;
    case 4:
      try
	{
	  int y = -1;
	  int *a = new int[y];
	  delete[] a;
	}
      catch (const std::bad_array_new_length &)
	{
	  return 5;
	}
      break;
    case 5:
      try
	{
	  int z = 1;
	  int *a = new int[z]{1, 2, 3};
	  delete[] a;
	}
      catch (std::bad_array_new_length &)
	{
	  return 6;
	}
      break;
    case 6:
      {
	std::exception_ptr b, d;
	if (b != nullptr || d != nullptr)
	  return -1;
	try
	  {
	    throw 1;
	  }
	catch (int a)
	  {
	    if (a != 1)
	      return -1;
	    b = std::current_exception ();
	    if (b == nullptr)
	      return -1;
	    try
	      {
		throw 2L;
	      }
	    catch (long int c)
	      {
		if (c != 2L)
		  return -1;
		d = std::current_exception ();
		if (d == nullptr || b == d)
		  return -1;
	      }
	    if (std::current_exception () != b)
	      return -1;
	  }
	if (std::current_exception () != nullptr)
	  return -1;
	try
	  {
	    std::rethrow_exception (d);
	  }
	catch (long int &e)
	  {
	    if (e != 2L)
	      return -1;
	    try
	      {
		std::rethrow_exception (b);
	      }
	    catch (const int &f)
	      {
		if (f != 1)
		  return -1;
		try
		  {
		    std::rethrow_exception (d);
		  }
		catch (const long int g)
		  {
		    if (g != 2L)
		      return -1;
		    try
		      {
			std::rethrow_exception (b);
		      }
		    catch (int h)
		      {
			if (h != 1)
			  return -1;
			std::exception_ptr i (b);
			std::exception_ptr j;
			if (j != nullptr || i == nullptr || i != b || bool (j))
			  return -1;
			j = i;
			if (j != b || !bool (j))
			  return -1;
			j = nullptr;
			std::swap (i, j);
			if (j == nullptr || j != b || i != nullptr)
			  return -1;
		      }
		  }
	      }
	  }
	return 7;
      }
    case 7:
      {
	std::exception_ptr a = std::make_exception_ptr (42);
	std::exception_ptr b = std::make_exception_ptr (std::exception ());
	std::exception_ptr c
	  = std::make_exception_ptr (std::bad_array_new_length ());
	try
	  {
	    std::rethrow_exception (a);
	  }
	catch (int d)
	  {
	    if (d != 42)
	      return -1;
	    try
	      {
		std::rethrow_exception (b);
	      }
	    catch (const std::exception &e)
	      {
		const char *f = e.what ();
		try
		  {
		    std::rethrow_exception (c);
		  }
		catch (const std::bad_alloc &g)
		  {
		    try
		      {
			throw;
		      }
		    catch (const std::bad_array_new_length &h)
		      {
			const char *i = h.what ();
			const char *j = g.what ();
		      }
		  }
	      }
	  }
	return 8;
      }
    case 8:
      {
	std::nested_exception a;
	if (a.nested_ptr () != nullptr)
	  return -1;
	try
	  {
	    std::nested_exception b;
	    if (b.nested_ptr () != nullptr)
	      return -1;
	    throw 42;
	  }
	catch (...)
	  {
	    std::nested_exception c;
	    if (c.nested_ptr () != std::current_exception ())
	      return -1;
	    std::nested_exception d = c;
	    if (d.nested_ptr () != c.nested_ptr ())
	      return -1;
	    c = d;
	    try
	      {
		c.rethrow_nested ();
	      }
	    catch (const int &e)
	      {
		if (e != 42)
		  return -1;
	      }
	  }
	return 9;
      }
    case 9:
      try
	{
	  std::throw_with_nested (I ());
	}
      catch (const std::nested_exception &a)
	{
	  if (a.nested_ptr () != nullptr)
	    return -1;
	  try
	    {
	      throw;
	    }
	  catch (const I &)
	    {
	      return 10;
	    }
	}
      return -1;
    case 10:
      try
	{
	  std::throw_with_nested (J ());
	}
      catch (const std::nested_exception &a)
	{
	  if (a.nested_ptr () != nullptr)
	    return -1;
	  try
	    {
	      throw;
	    }
	  catch (const J &)
	    {
	      return 11;
	    }
	}
      return -1;
    case 11:
      try
	{
	  std::throw_with_nested (K ());
	}
      catch (const std::nested_exception &)
	{
	  return -1;
	}
      catch (const K &)
	{
	  return 12;
	}
      return -1;
    case 12:
      try
	{
	  throw 42;
	}
      catch (...)
	{
	  I a;
	  try
	    {
	      std::rethrow_if_nested (a);
	    }
	  catch (const int &b)
	    {
	      if (b == 42)
		return 13;
	    }
	}
      return -1;
    case 13:
      try
	{
	  throw J ();
	}
      catch (const J &a)
	{
	  std::rethrow_if_nested (a);
	  return 14;
	}
      return -1;
    case 14:
      try
	{
	  throw 42;
	}
      catch (...)
	{
	  try
	    {
	      throw L ();
	    }
	  catch (const J &a)
	    {
	      try
		{
		  std::rethrow_if_nested (a);
		}
	      catch (const int &b)
		{
		  if (b == 42)
		    return 15;
		}
	    }
	}
      return -1;
    case 15:
      {
	std::rethrow_if_nested (1);
	M m;
	std::rethrow_if_nested (m);
	N n;
	std::rethrow_if_nested (n);
	O o;
	std::rethrow_if_nested (o);
	return 16;
      }
    default:
      break;
    }
  return -1;
}

static_assert (foo (0) == 1);
static_assert (foo (1) == 2);
static_assert (foo (2) == 3);
static_assert (foo (3) == 4);
static_assert (foo (4) == 5);
static_assert (foo (5) == 6);
static_assert (foo (6) == 7);
static_assert (foo (7) == 8);
static_assert (foo (8) == 9);
static_assert (foo (9) == 10);
static_assert (foo (10) == 11);
static_assert (foo (11) == 12);
static_assert (foo (12) == 13);
static_assert (foo (13) == 14);
static_assert (foo (14) == 15);
static_assert (foo (15) == 16);
static_assert (std::uncaught_exceptions () == 0);
