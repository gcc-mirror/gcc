// PR c++/88482
// { dg-do compile }

extern "C" int __cxa_throw (void *, void *, void (*) (void *));	// { dg-message "previous declaration" }
extern "C" void *__cxa_get_exception_ptr (void *) throw ();
extern "C" double __cxa_begin_catch (void *) throw ();		// { dg-message "previous declaration" }
extern "C" long *__cxa_end_catch () throw ();			// { dg-message "previous declaration" }
extern "C" char __cxa_rethrow ();				// { dg-message "previous declaration" }
extern "C" void __cxa_allocate_exception (__SIZE_TYPE__) throw ();// { dg-message "previous declaration" }
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
