 
int f()
{
  unsigned char hrs, min;

  min = ((min / 10) << 4) + min % 10;
  hrs = ((hrs / 10) << 4) + hrs % 10;

  return hrs + min;
}
