// { dg-do compile { target c++2a } }

void f1(int);
void f2(int) noexcept;

template<typename T>
concept C1 = requires (T t) { // { dg-message "in requirements" }
  { f1(t) } noexcept;
};

template<typename T>
concept C2 = requires (T t) {
  { f2(t) } noexcept;
};

template<C1 T>
void g1(T t);

template<C2 T>
void g2(T t);

void test() {
  g1(0); // { dg-error "" }
  g2(0);
}