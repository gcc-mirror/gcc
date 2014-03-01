int
foo (int cls, int sign)
{
  if (__builtin_expect (cls == 4, 0))
    return (sign
            ? (-((int) ((~(unsigned)0) >> 1)))-1
            : ((int) ((~(unsigned)0) >> 1)));
}
