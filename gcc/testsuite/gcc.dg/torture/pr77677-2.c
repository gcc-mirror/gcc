/* PR ipa/77677 */
/* { dg-do compile } */
/* { dg-options "-std=gnu17 -fpermissive -w" } */

enum machine_mode { MAX_MACHINE_MODE };

struct {
  int mode : 8;
} a;
int b;

static int fn1();

void fn2() { fn1(a, a.mode); }

int fn1(a, mode) enum machine_mode mode;
{ int c = b = c; }
