/* PR optimization/13031  */
/* The following code used to ICE on alphaev67-*-* at -O2 with an
   unrecognizable instruction, caused by local register allocation
   substituting a register for a constant in a conditional branch.  */

void emit(int, int);
int f(void);
static int signals[5];

static inline void select(int sel, void *klass)
{
  emit(klass ? 0 : f(), signals[sel ? 0 : 1]);
}

void all(void *gil, void *l, void *icon)
{
  while (l)
    if (icon)
      select(0, gil);
}

