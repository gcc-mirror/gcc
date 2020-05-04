// PR c++/88482
// { dg-do compile }

float __cxa_throw;		// { dg-message "previous declaration" }
extern "C" void *__cxa_get_exception_ptr (void *);
float __cxa_begin_catch;	// { dg-message "previous declaration" }
float __cxa_end_catch;		// { dg-message "previous declaration" }
float __cxa_rethrow;		// { dg-message "previous declaration" }
float __cxa_allocate_exception;	// { dg-message "previous declaration" }
extern "C" void __cxa_free_exception (void *);

struct S { S (); S (const S &); ~S (); };

int
foo (int x)
{
  if (x > 27)
    throw 19; // { dg-error "redeclared"  }
  try
    {
      if (x > 15)
	throw S ();
    }
  catch (S s) // { dg-error "redeclared"  }
    {
      throw;  // { dg-error "redeclared"  }
    }
  return x + 3;
}
