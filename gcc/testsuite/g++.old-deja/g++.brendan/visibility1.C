// Build don't link: 
// GROUPS passed visibility
class foo {
protected:
  int i; // ERROR - protected
};

class bar : public foo {
public:
  friend void baz (foo *);
};

void baz (foo *f)
{
  f->i = 1;	// error: i is protected// ERROR - .*
}
