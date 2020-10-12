/* { dg-do run } */

char a = 0, f = 0, c = 5;
unsigned long d = 0;
int g = 0;
int *e = &g;

int main() {
  char  b = 0;
  for (;;) {
    for (a = 0; a < 2; a++) { // no UB I believe
      if (c) {
        if (d != 0)
          __builtin_abort ();
        return 0;
      }
    }
    f = (d++, *e);
  }

  return 1;
}
