/* PR sanitizer/81923 */
/* { dg-do link } */

int foobar __asm (__USER_LABEL_PREFIX__ "barbaz") = 34;

int
main ()
{
  return 0;
}
