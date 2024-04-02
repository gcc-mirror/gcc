// PR c++/114562
// { dg-do compile { target c++11 } }

template <typename T>
struct Optional {
  Optional(T&&);
};

struct MyClass {
  MyClass(Optional<const void*>);
};

// const void* NONE = nullptr; // Correct Error
void* NONE = nullptr; // Crash

void beforeParam();

template<typename T>
struct Create {
  template <typename U> static T create(U &&) noexcept;
};


template <typename T>
template<typename U>
T Create<T>::create(U && u) noexcept {
  return T( ( (beforeParam()), (u) ) ); // { dg-error "cannot bind rvalue reference" }
  // return T( (u) ); // Correct Error
}

void test_func() {
  Create<MyClass>::create(NONE);
}
