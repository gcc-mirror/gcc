// PR c++/56998

class Secret;
char IsNullLiteralHelper(Secret* p);
char (&IsNullLiteralHelper(...))[2];

struct C
{
  int val() { return 42; }
};

template <typename T>
unsigned f()
{
  return sizeof(IsNullLiteralHelper(C().val()));
}
