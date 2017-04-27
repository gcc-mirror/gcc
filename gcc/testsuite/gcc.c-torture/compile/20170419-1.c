extern int __fpclassifyd (double x);

double fdim (double x, double y)
{
   int c = __fpclassifyd (x);
   if (c == 0)
     return (x);
   if (__fpclassifyd (y) == 0)
     return (y);
   if (c == 1)
     return (__builtin_huge_val ());
   return x > y ? x - y : 0.0;
}
