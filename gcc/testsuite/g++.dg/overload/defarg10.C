// PR c++/58063
// { dg-do run }

struct basic_ios
{
  bool operator!() const { return false; }
};

struct ostream : virtual basic_ios
{
};

int i;

ostream& operator<<(ostream& os, const char* s) {
  ++i;
  return os;
}

ostream cout;

void f(bool x = !(cout << "hi!\n")) { }

int main() {
  f();
  if (i != 1)
    __builtin_abort();
}
