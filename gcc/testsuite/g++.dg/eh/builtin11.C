// PR c++/88482
// { dg-do compile }

extern "C" void __cxa_throw (float, void *, void (*) (void *));	// { dg-message "previous declaration" }
extern "C" void *__cxa_get_exception_ptr (void *) throw ();
extern "C" void *__cxa_begin_catch (int) throw ();		// { dg-message "previous declaration" }
extern "C" void __cxa_end_catch (long long) throw ();		// { dg-message "previous declaration" }
extern "C" void __cxa_rethrow (int);				// { dg-message "previous declaration" }
extern "C" void *__cxa_allocate_exception (void *) throw ();	// { dg-message "previous declaration" }
extern "C" void __cxa_free_exception (void *) throw ();

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
      throw; // { dg-error "conflicting"  }
    }
  return x + 3;
}
