// Build don't link:

template<int N, class C>
class Bar {};

template<class C>
class Huh {};

template<int N>
void foo(const Bar<N,Huh<float[1]> > &x) {}

int main() {
  foo(Bar<3,Huh<float[1]> >());
}
