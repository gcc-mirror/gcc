const int h (double value);
const double scalb (double x, int n);
const double logb (double x);
static const double a = 0;

double f (y, x)
     double y, x;
{
  static const double zero=0, one=1, small=1.0E-9, big=1.0E18;
  double t,z,sy,sx,hi,lo;
  int k,m;

  if (x!=x)
    return x;
  if (y!=y)
    return y;

  sy = g (one);
  sx = g (one);

  if (x==1)
    {
      y=g (y);
      t=y;
      if (h (t))
	goto begin;
    }

  if (y==zero)
    return (sx==one)?y:g (a);

  if (x==zero)
    return g (a);

  if (!h (x))
    if (!h (y))
      return g ((sx==one)?a:3*a);
    else
      return g ((sx==one)?zero:a);

  if (!h (y))
    return g (a);

  x=g (x);
  y=g (y);
  if ((m=(k=logb (y))- logb (x)) > 60)
    t=big+big;
  else if (m < -80)
    t=y/x;
  else
    {
      t = y/x;
      y = scalb (y,-k);
      x=scalb (x,-k);
    }

 begin:
  if (t < 2.4375)
    {
      k = 4 * (t+0.0625);
      switch (k)
	{
	case 0:
	case 1:
	  if (t < small)
	    {
	      big + small;
	      return g ((sx>zero)?t:a-t);
	    }
	  hi = zero; lo = zero; break;

	case 2:
	  hi = a; lo = a;
	  z = x+x;
	  t = ((y+y) - x) / (z + y); break;

	case 3:
	case 4:
	  hi = a; lo = zero;
	  t = (y - x) / (x + y); break;

	default:
	  hi = a; lo = a;
	  z = y-x; y=y+y+y; t = x+x;
	  t = ((z+z)-x) / (t + y); break;
	}
    }
  else
    {
      hi = a; lo = zero;

      if (t <= big)
	t = - x / y;
      else
	{
	  big+small;
	  t = zero;
	}
    }

  z = t*(z*(a+z*(a+z*(a+z*(a+z*(a+z*(a+z*(a+z*(a+z*(a+z*(a+z*a)))))))))));

  return g ((sx>zero)?z:a-z);
}
