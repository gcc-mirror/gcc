// PR c++/88482
// { dg-do compile }

extern "C" void __cxa_throw (void *, void *, void (*) (void *));
extern "C" float __cxa_get_exception_ptr (void *) throw ();	// { dg-message "previous declaration" }
extern "C" void *__cxa_begin_catch (void *) throw ();
extern "C" void __cxa_end_catch ();
extern "C" void __cxa_rethrow ();
extern "C" void *__cxa_allocate_exception (__SIZE_TYPE__) throw ();
extern "C" int __cxa_free_exception (void *) throw ();		// { dg-message "previous declaration" }

struct S { S (); S (const S &); ~S (); };

int
foo (int x)
{
  if (x > 27)
    throw 19; // { dg-error "conflicting"  }
  try
    {
      if (x > 15)
	throw S ();
    }
  catch (S s) // { dg-error "conflicting"  }
    {
      throw;
    }
  return x + 3;
}
