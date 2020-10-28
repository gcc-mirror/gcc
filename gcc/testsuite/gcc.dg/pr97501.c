// { dg-do compile }
// { dg-options "-O2" }

static int c = 0;

int main() {
  int b = 0;
  if (c) {
  for (;; b--)
    do
      b++;
    while (b);
  }
}
