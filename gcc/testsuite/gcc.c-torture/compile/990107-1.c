static int
java_double_finite (d)
     double  d;
{
  long long  *ip = (long long  *) &d;
  return (*ip & 0x7ff0000000000000LL ) != 0x7ff0000000000000LL ;
}
