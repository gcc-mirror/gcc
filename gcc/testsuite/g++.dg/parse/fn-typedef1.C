// PR c++/40750

extern "C" void abort ();

typedef void Fn() const;

struct Foo {
  Fn fn;
};

bool called = false;
void Foo::fn() const { called = true; }

int main() {
  Foo f; f.fn();
  if (!called)
    abort();
}
