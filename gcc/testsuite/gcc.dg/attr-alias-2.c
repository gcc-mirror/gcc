/* PR 19031 */
/* { dg-do link } */
/* { dg-require-alias "" } */
/* { dg-options "-funit-at-a-time" } */

static int f1 (void) { return 0; }
extern int g1 (void) __attribute__((__alias__("f1")));

#define STR(x) STR1(__USER_LABEL_PREFIX__, x)
#define STR1(x,y) STR2(x, y)
#define STR2(x,y) #x #y

static int f2 (void) __asm__(STR(a2));
static int f2 (void) { return 0; }
extern int g2 (void) __attribute__((__alias__("a2")));

int main ()
{
  return g1() + g2();
}
