// PR c++/19367
// { dg-do link } 

void abort (void) { throw 3; }

namespace std { using ::abort; }

int main ()
{
  using std::abort;
  abort();
}
