// Build don't link:
// Origin: Carl Nygard <cnygard@bellatlantic.net>

template <class RT>
class Test { // ERROR - in instantiation
public:
  Test(const RT& c = RT()) {} // ERROR - reference to void
};

void f ()
{
  Test<void> c; // ERROR - instantiated from here
}


