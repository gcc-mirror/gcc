/* { dg-do compile */
/* { dg-options "-O2 -gdwarf-2 -dA" } */
/* { dg-final { scan-assembler "DW_TAG_variable" } } */
/* PR debug/21828 */

static int i;
int main() {
  i += 3;
  return 0;
}
