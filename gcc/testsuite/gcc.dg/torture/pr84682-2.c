/* { dg-do compile } */

int a;
void b() {
  float c;
  for (int d; d;)
    ;
  a = c;
  asm("" : : "pir"(c));
}
