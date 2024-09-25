/* { dg-do run } */

#define numwords 2

typedef struct {
  unsigned words[numwords];
} Child;

typedef struct {
  Child child;
} Parent;

Parent my_or(Parent x, const Parent *y) {
  const Child *y_child = &y->child;
  for (int i = 0; i < numwords; i++) {
    x.child.words[i] |= y_child->words[i];
  }
  return x;
}

int main() {
  Parent bs[4];
  __builtin_memset(bs, 0, sizeof(bs));

  bs[0].child.words[0] = 1;
  for (int i = 1; i <= 3; i++) {
    bs[i] = my_or(bs[i], &bs[i - 1]);
  }
  if (bs[2].child.words[0] != 1)
    __builtin_abort ();
  return 0;
}
