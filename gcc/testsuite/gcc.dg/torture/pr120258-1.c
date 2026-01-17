/* { dg-do run } */
/* PR tree-optimization/120258 */
/* SLSR would introduce an addition and multiplication
   that would have undefined behavior with respect to overflow
   and that would cause the following to be incorrect. */

int a = 1, b, c;
int main() {
  c = -__INT_MAX__* b - __INT_MAX__* a;
  if (b - __INT_MAX__ * a >= 0)
    __builtin_abort();
}
