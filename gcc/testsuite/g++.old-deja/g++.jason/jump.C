// PRMS Id: 6036

extern int a;

int main() {
  switch (a) {
  case 1:
    int v2 = 3;			// ERROR - referenced below
  case 2:			// ERROR - jumping past initializer
    if (v2 == 7)
      ;
  }
  return 0;
}
