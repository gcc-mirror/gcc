// { dg-do compile { target c++2a } }

struct string;

template<typename T>
concept C = string; // { dg-error "expected primary-expression" }

template<C T>
void fun(T s) { }

int main(int, char **) {
  fun((int *)0); // { dg-error "cannot call function" }
  return 0;
}

