/* { dg-do compile } */
/* { dg-options "-g -O2 -fno-var-tracking-assignments -gsplit-dwarf -g3" } */

/* Same as pr86064.c but compiled with -g3 it showed an issue in
   output_macinfo_op because of a typo in an assert.  */

int a;
__attribute__((__cold__)) void b();

void e(int *);
int f();

void c() {
  int d;
  e(&d);
  a = d;
  if (f())
    b();
}
