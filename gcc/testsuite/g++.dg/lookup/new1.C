// { dg-do compile }
// Origin: <igodard at rational dot com>
// PR c++/2518: operator new must not be looked up in local scope

int main() {
  int i;
  void* operator new(unsigned s, int* p);
  int* e = new(&i) int;                    // { dg-error "no matching function" }
  int* f = new int;
  return 0;
}

// { dg-excess-errors "operator new" }
