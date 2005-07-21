/* Test for PR target/20191.  */

struct S1;

struct S1 {
  struct S1 *next;
  float x;
};

struct S2 {
  float y;
};

extern int func_ex1 (float);

extern int f;
extern float n;
extern struct S1 *bp1;
extern struct S2 *bp2;

inline float
func1 (int f, struct S2 *p2)
{
  float a;

  if (f)
    a = n >= p2->y ? n : p2->y;
  else
    a = n;
  return a;
}

inline float
func2 (struct S1 *p1, struct S2 *p2)
{
  float a, b;

  if(n <= 1.0)
    b = func1 (f, p2);
  else
    {
       a = n <= p1->x ? 0.0 : p1->x;
       b = a >= p2->y ? a : p2->y;
    }
  return(b);
}

void
func3 (struct S1 *p)
{
  float a = 0.0;

  if (f)
    a = func2 (bp1, bp2);
  if (func_ex1 (a))
    bp1 = p;
}
