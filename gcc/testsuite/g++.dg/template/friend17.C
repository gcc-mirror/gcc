template <class T>
struct X {
  template <class U> void operator+=(U);
  
  template <class V>
  template <class U>
  friend void X<V>::operator+=(U);
};

int main() {   
  X<int>() += 1.0;
}
