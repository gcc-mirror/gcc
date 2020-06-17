// { dg-do compile { target c++2a } }

template<typename T>
  struct remove_reference
  { using type = T; };

template<typename T>
  using remove_reference_t = remove_reference<T>::type;

template<typename T>
  inline constexpr bool blah = false;

template<typename T>
  requires blah<remove_reference_t<T>>
  // { dg-message "typename remove_reference<T>::type" "" { target *-*-* } .-1 }
  void foo() { }

void bar() { foo<int> (); } // { dg-error "use of" }
