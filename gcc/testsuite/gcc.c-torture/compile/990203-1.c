int
f (f)
     float f;
{
  long long  *ip = (long long  *) &f;
  return (*ip & 0x7ff0000000000000LL ) != 0x7ff0000000000000LL ;
}
