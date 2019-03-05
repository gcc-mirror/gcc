// PR c++/88482
// { dg-do compile }

extern "C" void __cxa_throw (void *, void *, void (*) (void *));
extern "C" void *__cxa_get_exception_ptr (void *);
extern "C" void *__cxa_begin_catch (void *);
extern "C" void __cxa_end_catch ();
extern "C" void __cxa_rethrow ();
extern "C" void *__cxa_allocate_exception (__SIZE_TYPE__);
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
