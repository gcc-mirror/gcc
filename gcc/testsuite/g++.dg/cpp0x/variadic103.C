// { dg-options "-std=c++0x" }

template<class T>
T&& create();

template<class T, class... Args>
void test() {
  T t(create<Args>()...);	// { dg-error "unknown bound" }
  (void) t;
}

int main() {
  test<int[]>();
}
