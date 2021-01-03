/* { dg-do compile } */

typedef struct {
  char a;
  int b[];
} c;
int d, f;
c e;
void g() {
  int h, i, j;
  for (; i;)
    switch (i)
    case 4: {
      h = (__UINTPTR_TYPE__)g >= 3;
      for (; h; h -= 1)
        if (d)
          j = f;
    }
      for (; i < 3; i++)
        e.b[i] = j;
}
