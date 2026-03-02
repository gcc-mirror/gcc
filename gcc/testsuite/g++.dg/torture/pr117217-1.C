struct [[gnu::packed]] A {
  int i;
  bool b;
};

struct [[gnu::packed]] B {
  int i;
  bool b : 1;
};

struct E {
  union Data {
    A a;
    B b;
    Data(const B &b) : b(b) {}
  } data;
};

extern B copy;

int main() {
  E e{{B()}};
  copy = e.data.b; // NEEDED FOR ICE
}
