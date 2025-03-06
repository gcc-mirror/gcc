/* { dg-do compile } */

int a, b;

void func2(short);

void func1() {
  while (1) {
    int loc = 8;
    while (1) {
      func2(loc);
      if (a)
        loc = 3;
      else if (b)
        break;
      loc |= a;
    }
  }
}
