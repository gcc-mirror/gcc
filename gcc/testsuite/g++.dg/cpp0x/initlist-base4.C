// PR c++/65816
// { dg-do run { target c++11 } }

void* operator new(decltype(sizeof(int)), void* ptr) { return ptr; }

struct item { int i; };

struct collector : item {
  int j;
  collector() = default;
  collector(int) {}
};

struct tuple : collector {
  tuple() : collector() {}
};

int main() {
  alignas(tuple) unsigned char space[sizeof(tuple)];
  for (auto& c : space) c = 0xff;

  auto ptr = ::new(&space) tuple;
  int& i = static_cast<tuple&>(*ptr).i;
  int& j = static_cast<tuple&>(*ptr).j;
  if (i != 0 || j != 0) __builtin_abort();
}
