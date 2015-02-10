// PR middle-end/64937
// { dg-do compile }
// { dg-options "-fsanitize=address -fcompare-debug" }

namespace foo_aux {
  struct BarParser { };
}
extern "C" {
  extern void __assert_fail (__const char *__assertion, __const char *__file,
                             unsigned int __line, __const char *__function);
}
namespace foo {
  class BarBox {
  public:
    BarBox (int xl = 0, int yl = 0) { }
  };
  class BarFoo {
  public:
    explicit BarFoo (BarBox box) {
      ((_orig_mask) ? static_cast < void >(0) :
       __assert_fail ("_orig_mask", "foo.h", 159, __PRETTY_FUNCTION__));
    }
    BarBox *_orig_mask;
  };
}
static void
ProcessOp (foo_aux::BarParser * p, int xl, int yr)
{
  foo::BarFoo tiles (foo::BarBox (xl, yr));
}
