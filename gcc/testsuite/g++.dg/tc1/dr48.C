// { dg-do link }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR48: Definitions of unused static members 

struct A {
  static const int size = 10;
  int array[size];
};

int main() {
  A a;
  return 0;
}
