/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

typedef struct {
  double a, b;
} c;
typedef struct {
  c d;
  long coordinates;
} e;
int f;
c g;
e h;
void k(int);
int n();
void j() { int i; k(i); }
void k(int l) {
  double a;
  int b;
  c m[4];
  long i;
  for (; l;)
    do {
      g.a = b ?: a;
      m[3] = g;
      if (f)
        m[0] = m[1] = m[3];
      i = 0;
      for (; i < 4; i++)
        (&h + i)->d = m[i];
    } while (n());
}
