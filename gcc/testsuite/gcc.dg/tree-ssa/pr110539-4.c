/* { dg-do compile } */
/* { dg-options "-O1 -fstrict-aliasing -fdump-tree-optimized" } */

/* This is a small variant of pr110539-3.c using -O1 -fstrict-aliasing
   rather than -O2. Just to show VRP and PRE is not needed to optimize
   the call to foo away. */


void foo(void);
static int a, c = 1;
static short b;
static int *d = &c, *e = &a;
static int **f = &d;
void __assert_fail() __attribute__((__noreturn__));
static void g(int h) {
    if (*d)
        ;
    else {
        if (e) __assert_fail();
        if (a) {
            __builtin_unreachable();
        } else
            __assert_fail();
    }
    if (((h!=0) == h) + b) *f = 0;
}

int main() {
    int i = 0 != 10 & a;
    g(i);
    *e = 9;
    e = 0;
    if (d == 0)
        ;
    else
        foo();
    ;
}

/* The call to foo should be optimized away. */
/* The missed optimization at -O1 here was:
        int b = a & 1;
        int c = b != 0;
        int d = c == b;
  not being optimized to 1 early enough, it is done in vrp2 but
  that is too late.
  In phiopt2 we got:
    _17 = i_7 != 0;
    _12 = (int) _17;
    if (i_7 == _12)
      goto <bb 9>; [50.00%]
    else
      goto <bb 10>; [50.00%]

    <bb 9> [local count: 268435456]:
    d = 0B;

    <bb 10> [local count: 536870913]:
    e.1_3 = e;
    *e.1_3 = 9;
    e = 0B;
    d.2_4 = d;
    if (d.2_4 == 0B)

  The first if is not optimized before, until vrp2 which is
  too late as there are no passes which will then find the
  load of d in `d.2_4 = d;` was `0B` after vrp2.

  Now in forwprop3 (after phiopt2), we optimize:
    _17 = i_7 != 0;
    _12 = (int) _17;
    if (i_7 == _12)
  into just:
    _t = (unsigned)i_7;
    if (_t <= 1)

  And then during ccp3, that is optimized away and that is optimized
  early enough now that the load `d.2_4 = d;` is optimizd to just
 `d.2_4 = 0B;`
 */

/* { dg-final { scan-tree-dump-not "foo \\(\\)" "optimized"} } */
