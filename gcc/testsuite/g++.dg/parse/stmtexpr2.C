// { dg-do compile }
// { dg-options "" }

#define DMAX(a,b) ({double _a = (a), _b = (b); _a > _b ? _a : _b; })

void foo(void)
{
  double xl, dy;
  xl = DMAX(dy, 0.0);
}

