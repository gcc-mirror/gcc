// { dg-do run }

struct S {
  static const int i = 3;
};

const int S::i;

int main () {
  if (!S::i)
    return 1;
}
