// PR c++/88482
// { dg-do compile }

extern "C" int __cxa_throw (void *, void *, void (*) (void *));	// { dg-error "declared incorrectly" }
extern "C" void *__cxa_get_exception_ptr (void *);
extern "C" double __cxa_begin_catch (void *);			// { dg-error "declared incorrectly" }
extern "C" long *__cxa_end_catch ();				// { dg-error "declared incorrectly" }
extern "C" char __cxa_rethrow ();				// { dg-error "declared incorrectly" }
extern "C" void __cxa_allocate_exception (__SIZE_TYPE__);	// { dg-error "declared incorrectly" }
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
