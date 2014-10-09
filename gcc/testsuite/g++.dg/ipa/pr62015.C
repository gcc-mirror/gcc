/* { dg-do run } */
/* { dg-options "-O3 -std=c++11"  } */


extern "C" int printf(const char *fmt, ...);
extern "C" void abort(void);

struct Side {
    enum _Value { Left, Right, Invalid };

    constexpr Side() : _value(Invalid) {}
    constexpr Side(_Value value) : _value(value) {}
    operator _Value() const { return (_Value)_value; }

  private:
    char _value;
};

struct A {
    void init();
    void adjust(Side side, bool final);
    void move(Side side);
};

void A::init()
{
    adjust(Side::Invalid, false);
}

static void __attribute__((noinline))
check (int v, int final)
{
    if (v != 0)
      abort();
}


__attribute__((noinline))
void A::adjust(Side side, bool final)
{
  check ((int)side, final);
}

void A::move(Side side)
{
    adjust(side, false);
    adjust(side, true);
}

int main()
{
    A t;
    t.move(Side::Left);
    return 0;
}
