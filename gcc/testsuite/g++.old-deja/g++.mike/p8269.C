// Build don't link:
// prms-id: 8269

typedef char* const char_ptr;

class Foo_Bar {
public:
  static char* const counter;
};

char_ptr Foo_Bar::counter = 0;
