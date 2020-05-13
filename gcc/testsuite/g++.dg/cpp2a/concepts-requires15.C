// { dg-do compile { target c++20 } }

struct string;

template<typename T>
concept C = string; // { dg-error "expected primary-expression" }

template<C T>
void fun(T s) { }

int main(int, char **) {
  fun((int *)0); // { dg-error "" }
  return 0;
}

