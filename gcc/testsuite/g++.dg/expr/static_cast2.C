struct B {};

int main () {
  B a;
  (1 ? static_cast<B&>(a) :
       *static_cast<B*>(&a));
}
