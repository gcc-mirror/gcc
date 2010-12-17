// { dg-options "-std=c++0x" }

struct nAny {
  template<class... T>
  nAny(T&&...);
};

template<class T>
T&& create();

template<class T, class... Args>
void test() {
  T t(create<Args>()...);
  (void) t;
}

int main() {
  test<nAny>();
}
