// PR c++/88482
// { dg-do compile }

extern "C" void __cxa_throw (float, void *, void (*) (void *));	// { dg-error "declared incorrectly" }
extern "C" void *__cxa_get_exception_ptr (void *);
extern "C" void *__cxa_begin_catch (int);			// { dg-error "declared incorrectly" }
extern "C" void __cxa_end_catch (long long);			// { dg-error "declared incorrectly" }
extern "C" void __cxa_rethrow (int);				// { dg-error "declared incorrectly" }
extern "C" void *__cxa_allocate_exception (void *);		// { dg-error "declared incorrectly" }
extern "C" void __cxa_free_exception (void *);

struct S { S (); S (const S &); ~S (); };

int
foo (int x)
{
  if (x > 27)
    throw 19;
  try
    {
      if (x > 15)
	throw S ();
    }
  catch (S s)
    {
      throw;
    }
  return x + 3;
}
