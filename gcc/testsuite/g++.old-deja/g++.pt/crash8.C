// Build don't link:

template<class T>
class TestClass1 {
public:
  TestClass1() { } 
};

template<class T>
class TestClass2 {
public:
  TestClass2() { } 
  T operator()(int) { }
};

template<class T>
void doit(T x) {
  TestClass1<T> q1;
  q1 = TestClass1<T>();
  TestClass2<T> q2;
  q2 = TestClass2<T>();

  TestClass1<T> p1;
  p1 = TestClass1(); // ERROR - template used as expression

  TestClass2<T> p2;
  p2 = TestClass2(); // ERROR - template used as expression
}

int main() {
  double x;
  doit(x);
}

