/* { dg-do compile } */

int a, b, d;
void e() {
  do {
    int f = 0;
    while (1) {
      int c = a;
      for (; (c & 1) == 0; c = 1)
        for (; c & 1;)
          ;
      if (a)
        break;
      f++;
    }
    b = f & 5;
    if (b)
      break;
  } while (d++);
}
