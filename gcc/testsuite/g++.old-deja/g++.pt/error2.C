// { dg-do assemble  }
// Origin: Carl Nygard <cnygard@bellatlantic.net>

template <class RT>
class Test { // { dg-error "" } in instantiation
public:
  Test(const RT& c = RT()) {} // { dg-error "" } reference to void
};

void f ()
{
  Test<void> c; // { dg-error "" } instantiated from here
}


