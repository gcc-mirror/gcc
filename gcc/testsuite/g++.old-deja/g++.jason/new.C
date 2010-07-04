// { dg-do assemble  }
// Bug: new doesn't make sure that the count is an integral value.

#include <new>
extern "C" int printf (const char *, ...);
extern "C" void *malloc (std::size_t);
std::size_t s;

void * operator new (std::size_t siz) throw (std::bad_alloc) {
  if (s == 0)
    s = siz;
  else
    s = (s != siz);
  return malloc (siz);
}

int main()
{
  s = 0;

  float f = 3;
  int* b1 = new int[(int)f];
  int* b2 = new int[f];		// { dg-error "" } new requires integral size

  return s;
}
