// { dg-options "" }

template <class T>
void test1() {
  int x = 0;
  const typeof(x) & t1 = x+0;
}

void test2() {
  int x = 0;
  const typeof(x) & t1 = x+0;
}

int main() {
  test1<int>();
  test2 ();
}
