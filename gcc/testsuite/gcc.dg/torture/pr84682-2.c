/* { dg-do compile } */
/* { dg-skip-if "impossible register constraint" { "avr-*-*" } } */

int a;
void b() {
  float c;
  for (int d; d;)
    ;
  a = c;
  asm("" : : "pir"(c));
}
