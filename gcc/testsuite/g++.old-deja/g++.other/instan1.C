// { dg-do link  }
// { dg-options "-fno-implicit-templates" }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct U {
  static int j;
};

template <class T>
struct S {
  static const int i = 7;
};

template <class T>
const int S<T>::i;

template <class T>
int U<T>::j = S<T>::i + 5;

template int U<double>::j;

int main () {
}
