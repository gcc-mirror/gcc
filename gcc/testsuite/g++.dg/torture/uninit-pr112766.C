// { dg-do compile }
// { dg-additional-options "-Wuninitialized" }

void* operator new[](__SIZE_TYPE__, void* __p) ;

class Result
{
public:
  Result();
  ~Result();
};

void *foo(long nElements, void *p)
{
  return p ? new((int*)p) Result[nElements] : new Result[nElements]; // { dg-bogus "uninitialized" }
}

