// PR c++/88482
// { dg-do compile }

float __cxa_throw;		// { dg-error "declared incorrectly" }
extern "C" void *__cxa_get_exception_ptr (void *);
float __cxa_begin_catch;	// { dg-error "declared incorrectly" }
float __cxa_end_catch;		// { dg-error "declared incorrectly" }
float __cxa_rethrow;		// { dg-error "declared incorrectly" }
float __cxa_allocate_exception;	// { dg-error "declared incorrectly" }
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
