// prms-id: 4104

template <class T>
void F(T &a, void (*P)(T &temp)) {
  (*P)(a);
}

template <class T>
void G(T &a) {
}

int main() {
  int a;
  F(a, G);
}
