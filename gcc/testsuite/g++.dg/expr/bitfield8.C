// PR c++/31513
// { dg-do run }

extern "C" void abort();

struct tree_type {
  unsigned int precision : 9;
};

void bork(unsigned int i) {
  if (i != 7)
    abort();
}

void foo(struct tree_type *t)
{
  bork(t->precision);
}

int main() {
  tree_type t;
  t.precision = 7;
  foo(&t);
}
