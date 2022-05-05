// PR c++/66139
// { dg-do run { target c++11 } }

int constructed = 0;

class lock_guard_ext{
public:
  lock_guard_ext() { ++constructed; }
  ~lock_guard_ext() { --constructed; }
};
 
struct Access {
  lock_guard_ext lock;
  int value;
};
 
int t() {
  throw 0;
}

Access foo1() {
  return { {}, t() };
}
 
int main () {
  try { foo1(); } catch (int) {}
  if (constructed != 0)
    __builtin_abort();
}
