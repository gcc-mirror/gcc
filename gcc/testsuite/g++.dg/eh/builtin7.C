// PR c++/88482
// { dg-do compile }
// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.

extern "C" void __cxa_throw (void *, void *, void (*) (void *));
int __cxa_get_exception_ptr;		// { dg-message "previous declaration" }
extern "C" void *__cxa_begin_catch (void *) throw ();
extern "C" void __cxa_end_catch ();
extern "C" void __cxa_rethrow ();
extern "C" void *__cxa_allocate_exception (__SIZE_TYPE__) throw ();
int __cxa_free_exception;		// { dg-message "previous declaration" }

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
      throw;
    }
  return x + 3;
}
