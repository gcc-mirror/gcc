// { dg-do assemble  }
//Based on a report by Bill Currie <bcurrie@tssc.co.nz>
struct foo {
  protected:
    int x;        // { dg-error "" } candidate
};

struct bar {
  public:
    int x();      // { dg-error "" } candidate
};

struct foobar: public foo, public bar {
  foobar();
};

int func(int);

foobar::foobar()
{
    func(x);       // { dg-error "" } ambiguous member access
}
