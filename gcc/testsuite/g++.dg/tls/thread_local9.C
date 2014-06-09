// PR c++/61343

// { dg-do run { target c++11 } }
// { dg-add-options tls }
// { dg-require-effective-target tls_runtime }

struct Foo {
  int value;

  Foo() noexcept {
    value = 12;
  }
};

static thread_local Foo a{};

static __attribute__((noinline)) void UseA() {
  if (a.value != 12) __builtin_abort();
}

int main() {
  UseA();
}
