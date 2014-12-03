// { dg-do assemble  }
// PRMS Id: 6036

extern int a;

int main() {
  switch (a) {
  case 1:
    int v2 = 3;			// { dg-message "" } referenced below
  case 2:			// { dg-error "" } jumping past initializer
    if (v2 == 7)
      ;
  }
  return 0;
}
