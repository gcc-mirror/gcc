// { dg-do link }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR164: Overlap between Koenig and normal lookup 

void f(int);

template <class T> void g(T t) {
  f(t);
}

enum E { e };

void f(E) {}

int main() {
  g(e);
}
