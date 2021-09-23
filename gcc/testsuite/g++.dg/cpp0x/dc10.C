// PR c++/65816
// { dg-do run { target c++11 } }

void* operator new(decltype(sizeof(int)), void* ptr) { return ptr; }

struct test {
  int i;
  test() = default;
  test(int) : test() {}
};

int main() {
  alignas(test) unsigned char space[sizeof(test)];
  for (auto& c : space) c = 0xff;

  auto ptr = ::new(&space) test(42);
  int& i = static_cast<test&>(*ptr).i;
  if (i != 0) __builtin_abort();
}
