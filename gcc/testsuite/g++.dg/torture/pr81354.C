// PR81354 reported this test as crashing in a limited range of revisions.
// { dg-do compile }

struct T { double a; double b; };

void foo(T Ad[], int As[2])
{
  int j;
  int i;
  int Bs[2] = {0,0};
  T Bd[16];

  for (j = 0; j < 4; j++) {
    for (i = 0; i + 1 <= j + 1; i++) {
      Ad[i + As[0] * j] = Bd[i + Bs[0] * j];
    }

    i = j + 1;  // <- comment out this line and it does not crash
    for (; i + 1 < 5; i++) {
      Ad[i + As[0] * j].a = 0.0;
      Ad[i + As[0] * j].b = 0.0;
    }
  }
}
