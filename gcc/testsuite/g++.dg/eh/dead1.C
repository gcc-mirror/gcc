// PR 6320
// Rechained the MUST_NOT_THROW region in the wrong order wrt the 
// TRY/CATCH while removing them and got confused.
// { dg-do compile }

struct S {
  ~S();
};

void foo()
{
  try {
    return;
  }
  catch (int) {
  }
  catch (...) {
    S s;
  }
}
