// { dg-options "-Wmissing-declarations" }

void fn1() { }		// { dg-warning "no previous declaration" }
namespace ns {
  void fn2() { }	// { dg-warning "no previous declaration" }
}
namespace {
  void fn3() { }
}
static void fn4() { }

void fn5();
namespace ns {
  void fn6();
}

void fn5() { }
namespace ns {
  void fn6() { }
}

inline void fn7() { }

class c {
  void cfn1() { }
  static void cfn2() { }
  void cfn3();
  static void cfn4();
};

void c::cfn3() { }
void c::cfn4() { }

static struct {
  void sfn1() { }
  static void sfn2() { }
} s;

template<typename C>
void tfn1() { }

template void tfn1<c>();

class d { };
template<> void tfn1<d>() { }
