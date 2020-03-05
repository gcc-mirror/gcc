// { dg-do run }
// { dg-additional-options "-fstrict-aliasing" }

template <typename = void> struct Optional {
  auto is_present() const { const bool &p = inner.present; return p; }
  auto set_present() { if (not is_present()) inner.present = true; }
  struct InnerType {
    bool present = false;
    char padding[1] = {0};
  };
  using inner_t = InnerType;
  inner_t inner = {};
};

template <typename WrappedType> struct Wrapper {
  auto operator-> () { return value; }
  WrappedType *value;
};

void __attribute__((noipa)) foo(Optional<>& x) {}

int main()
{
  Optional<> buf{};
  foo(buf);
  Wrapper<Optional<>> wo = {&buf};
  wo->set_present();
  auto x = wo->is_present();
  if (!x)
    __builtin_abort ();
}
