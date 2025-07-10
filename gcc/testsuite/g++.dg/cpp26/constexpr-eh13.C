// C++26 P3068R5 - Allowing exception throwing in constant-evaluation
// { dg-do compile { target c++26 } }
// { dg-require-effective-target exceptions_enabled }

template <typename T>
consteval T
foo (T x)
{
  try
    {
      throw &x;
    }
  catch (void *ptr)		// { dg-message "for type 'void\\\*'" }
    {
      return *static_cast<T *> (ptr) | 0x10;
    }
  catch (const void *ptr)	// { dg-message "for type 'const void\\\*'" }
    {
      return *static_cast<const T *> (ptr) | 0x20;
    }
  catch (T *ptr)		// { dg-warning "exception of type 'T\\\*' will be caught by earlier handler" }
    {				// { dg-warning "exception of type 'int\\\*' will be caught by earlier handler" "" { target *-*-* } .-1 }
      return *ptr | 0x30;	// { dg-warning "exception of type 'long long unsigned int\\\*' will be caught by earlier handler" "" { target *-*-* } .-2 }
    }
  catch (const T *ptr)
    {
      return *ptr | 0x40;
    }
  catch (...)
    {
      return -1;
    }
}

static_assert (foo (1) == 0x11);
static_assert (foo (2ULL) == 0x12ULL);
