extern void abort (void);

long long 
f (long long a) 
{ 
  return (a << 32) >> 32; 
} 
long long a = 0x1234567876543210LL;
long long b = (0x1234567876543210LL << 32) >> 32;
int
main ()
{
  if (f (a) != b)
    abort ();
  return 0;
}
