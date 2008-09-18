// { dg-do assemble  }
// Origin: Carl Nygard <cnygard@bellatlantic.net>

template <class RT>
class Test {
public:
  Test(const RT& c = RT()) {} // { dg-error "reference to void" } 
};

void f ()
{
  Test<void> c; // { dg-message "instantiated" }
}


