/* { dg-do compile { target { *-*-darwin* } } } */
/* { dg-options "-O1" } */

typedef unsigned long long tick_t;
extern int foo(void);
extern tick_t tick(void);
double test(void) {
  struct { tick_t ticks; } st;
  st.ticks = tick();
  foo();
  return (double)st.ticks;
}
