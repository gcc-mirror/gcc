//Build don't link:
//Based on a report by Bill Currie <bcurrie@tssc.co.nz>
struct foo {
  protected:
    int x;
};

struct bar {
  public:
    int x();
};

struct foobar: public foo, public bar {
  foobar();
};

int func(int);

foobar::foobar()
{
    func(x);       // ERROR - ambiguous member access
}
