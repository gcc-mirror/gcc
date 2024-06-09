/* { dg-do run } */

struct a {
  int b;
} c;

int d;
long e;

static void f(char *g, char *h, struct a *l) {
  char a[1024];
  int j = 0;

  if (d)
    h = a;

  for (; g < h; g++)
    if (__builtin_iscntrl(*g))
      ++j;

  while (l->b < j)
    ;
}

int main() {
  static const struct {
    char *input;
  } k[] = {{"somepage.html"}, {""}};

  for (unsigned int i = 0; i < 1; ++i) {
    e = __builtin_strlen(k[i].input);
    f(k[i].input, k[i].input + e, &c);
  }
}
