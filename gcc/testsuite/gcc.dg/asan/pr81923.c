/* PR sanitizer/81923 */
/* { dg-do link } */

#define STR1(X) #X
#define STR2(X) STR1(X)

int foobar __asm (STR2(__USER_LABEL_PREFIX__) "barbaz") = 34;

int
main ()
{
  return 0;
}
