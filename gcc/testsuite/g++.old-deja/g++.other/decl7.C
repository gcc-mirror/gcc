// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S 
{
  void* sp;
};

void* f ()
{
  struct S s = { &s.sp };
  return s.sp;
}
