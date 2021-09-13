/* { dg-do compile } */
/* { dg-options "-fsanitize=address" } */

int f(int i) {
  auto int h() {
    int r;
    int *p;

    {
      int x[3];

      auto int g() {
	return x[i];
      }

      p = &r;
      *p = g();
    }

    return *p;
  }

  return h();
}
