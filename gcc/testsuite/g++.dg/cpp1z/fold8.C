// PR c++/68377
// { dg-options -std=c++17 }

struct Sink { } s;
template <class T> Sink& operator<<(Sink&, const T&);

template<class... Tx> 
int f(Tx... xs) {
  return ((xs+1) + ...);
}

int main() {
  s << f(3,4,5) << "\n";
  return 0;
}
