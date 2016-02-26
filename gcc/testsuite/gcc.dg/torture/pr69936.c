/* { dg-do compile } */

int a;
char b;
void fn1(int p1) {}

int fn2() { return 5; }

void fn3() {
  if (fn2())
    ;
  else {
    char c[5];
    c[0] = 5;
  lbl_608:
    fn1(c[9]);
    int d = c[9];
    c[2] | a;
    d = c[b];
  }
  goto lbl_608;
}

int main() { return 0; }
