/* { dg-do run } */
/* { dg-options "-O" } */

extern void abort (void);

typedef struct {
  short x;
} mytype_t;

mytype_t *__attribute__  ((noinline,weak))
some_func (void)
{
  static mytype_t s;
  return &s;
};

int main (void)
{
  int y, y2;
  mytype_t *shorter = some_func();
  y = __builtin_arc_aligned (shorter, 2);
  if (!y)
    abort ();
  y2 = __builtin_arc_aligned (shorter, 4);
  if (y2)
    abort ();
  return 0;
}
