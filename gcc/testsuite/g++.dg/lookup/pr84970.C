// PR c++/84970 ICE with deferred initializer

namespace bob {
  void a();
}
using namespace bob;

void a (int);

template <typename b>
void *x (b)
{
  void (*c)(b) (a);

  return (void *)c;
}

void d () {
  x (1);
}

