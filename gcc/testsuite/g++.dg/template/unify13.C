// PR c++/120161

template<class T, class U>
struct mp_list { };

template<class T>
struct Wrap { struct type { }; };

struct A : mp_list<Wrap<int>::type, void>
         , mp_list<Wrap<long>::type, void> { };

template<class U>
void f(mp_list<Wrap<int>::type, U>*);

int main() {
  A a;
  f(&a);
}
