// PR c++/61402
// { dg-do run { target c++14 } }

extern "C" void abort();

template<typename T>
void foo(T t) {
  auto test = [ i = ++t ](T v) {
    if (i != v)
      abort();
  };
  test(t);
}

int main(){
  foo(3.14f);
  foo(0);
  foo('a');
}
