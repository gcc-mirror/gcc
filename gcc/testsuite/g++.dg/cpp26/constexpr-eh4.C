// C++26 P3068R5 - Allowing exception throwing in constant-evaluation
// { dg-do compile { target c++26 } }
// { dg-require-effective-target exceptions_enabled }

struct A { virtual ~A () {} };
struct B { virtual void b (); };
struct C { virtual void c (); };
struct D : private B { virtual void d (); };
struct E { virtual void e (); };
struct F : D, E, private C { virtual void f (); };

constexpr int
foo (int x)
{
  switch (x)
    {
    case 1:
      try
	{
	  static constexpr F f;
	  D &d = dynamic_cast<D &>((B &) f);	// { dg-error "called without 'std::bad_cast' being defined" }
	  return -1;
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
	  char (*a)[2] = new char[x][2];	// { dg-error "called without 'std::bad_array_new_length' being defined" }
	  delete[] a;
	}
      catch (...)
	{
	  return -1;
	}
      break;
    case 4:
      try
	{
	  int y = -1;
	  int *a = new int[y];			// { dg-error "called without 'std::bad_array_new_length' being defined" }
	  delete[] a;
	}
      catch (...)
	{
	  return -1;
	}
      break;
    case 5:
      try
	{
	  int z = 1;
	  int *a = new int[z]{1, 2, 3};		// { dg-error "called without 'std::bad_array_new_length' being defined" }
	  delete[] a;
	}
      catch (...)
	{
	  return -1;
	}
      break;
    }
  return -1;
}

constexpr int a = foo (1);			// { dg-message "in 'constexpr' expansion of" }
constexpr int b = foo (3);			// { dg-message "in 'constexpr' expansion of" }
constexpr int c = foo (4);			// { dg-message "in 'constexpr' expansion of" }
constexpr int d = foo (5);			// { dg-message "in 'constexpr' expansion of" }
