// PR c++/65209
// { dg-additional-options "-fno-pie" { target { ia32 || sparc*-*-* } } }
// { dg-final { scan-assembler-not "comdat" } }

// Everything involving the anonymous namespace bits should be private, not
// COMDAT.

struct Bar
{
  static Bar *self();
  char pad[24];
};

template <Bar *(&holderFunction)()>
struct BarGlobalStatic
{
  Bar *operator()() { return holderFunction(); }
};

namespace {
  namespace Q_QGS_s_self {
    inline Bar *innerFunction() {
      static struct Holder {
	Bar value;
	~Holder() {}
      } holder;
      return &holder.value;
    }
  }
}
static BarGlobalStatic<Q_QGS_s_self::innerFunction> s_self;

Bar *Bar::self()
{
  return s_self();
}

int main(int argc, char *argv[])
{
  Bar* bar = Bar::self();
  return 0;
}
