// PR c++/94481
// { dg-do compile { target c++20 } }

template<typename T>
concept C = true;

void foo() {
  C decltype c = 1;		// { dg-error "" }
}
