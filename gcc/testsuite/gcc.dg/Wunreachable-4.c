/* PR middle-end/10336 */
/* { dg-options "-Wunreachable-code" } */

void foo(int i)
{
  switch(i) {
    case 0:
      break;
    case 1:
      break;
  }
}
