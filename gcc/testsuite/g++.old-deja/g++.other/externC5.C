// { dg-do assemble  }
// Origin: schmid@snake.iap.physik.tu-darmstadt.de

extern "C" int rand (void) throw ();

namespace std
{ 
extern "C" int rand(void) throw(); 
template <class T> void f(T a) {}
}

using namespace std;

int main()
{
  f(rand);
  f(std::rand);
  f(::rand);
}
