/* PR c/10175 */
/* { dg-do compile } */
/* { dg-options "-Wunreachable-code" } */

int i,j;
int main(void)
{
  if (0) {
    i = 0;		   /* { dg-warning "will never be executed" "" } */
    j = 0;
  } else {
    i = 1;
    j = 1;
  }

  return 0;
}
