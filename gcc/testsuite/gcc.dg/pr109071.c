/* PR tree-optimization/109071 need more context for -Warray-bounds warnings
   due to code duplication from jump threading.  */  
/* { dg-options "-O2 -Wall -fdiagnostics-show-context=1" } */
/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */

extern void warn(void);
static inline void assign(int val, int *regs, int index)
{
  if (index >= 4)
    warn();
  *regs = val;
}
struct nums {int vals[4];};

void sparx5_set (int *ptr, struct nums *sg, int index)
{
  int *val = &sg->vals[index]; /* { dg-warning "is above array bounds" } */

  assign(0,    ptr, index);
  assign(*val, ptr, index);
}
/* { dg-begin-multiline-output "" }
   NN |   int *val = &sg->vals[index];
      |               ~~~~~~~~^~~~~~~
  'sparx5_set': events 1-2
   NN |   if (index >= 4)
      |      ^
      |      |
      |      (1) when the condition is evaluated to true
......
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
      |               ~~~~~~~~~~~~~~~
      |                       |
      |                       (2) warning happens here
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   NN | struct nums {int vals[4];};
      |                  ^~~~
   { dg-end-multiline-output "" } */
