// This testcase failed on s390, because cross-jumping merged 2 calls,
// one with REG_EH_REGION note with no handlers (ie. termination)
// and one without REG_EH_REGION note.
// { dg-do run }
// { dg-options "-O2" }

#include <exception>
#include <string>

struct E : public std::exception
{
  std::string m;
  E () : m ("test") { }
  ~E () throw() { }
};

struct C : public E { };

void foo ()
{
  throw C ();
}

int main ()
{
  try
    {
      foo ();
    }
  catch (...) { }
}
