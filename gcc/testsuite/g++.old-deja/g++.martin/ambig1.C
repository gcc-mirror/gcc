//Build don't link:
//Based on a report by Bill Currie <bcurrie@tssc.co.nz>
struct foo {
  protected:
    int x;        // ERROR - candidate
};

struct bar {
  public:
    int x();      // ERROR - candidate
};

struct foobar: public foo, public bar {
  foobar();
};

int func(int);

foobar::foobar()
{
    func(x);       // ERROR - ambiguous member access
}
