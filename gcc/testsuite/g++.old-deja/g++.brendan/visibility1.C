// { dg-do assemble  }
// GROUPS passed visibility
class foo {
protected:
  int i; // { dg-error "" } protected
};

class bar : public foo {
public:
  friend void baz (foo *);
};

void baz (foo *f)
{
  f->i = 1;	// error: i is protected// { dg-error "" } .*
}
