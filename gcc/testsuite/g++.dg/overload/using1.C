void f();

namespace N { 
  using ::f;
}

bool b;

void g() {
  b = N::f == ::f;
}
