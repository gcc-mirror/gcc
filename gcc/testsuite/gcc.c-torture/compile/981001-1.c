unsigned short code = 0x0000;
unsigned short  low = 0x4000;
unsigned short high = 0xb000;

int main (void)
{
  if (
         (high & 0x8000) != (low & 0x8000)
      && ( low & 0x4000) == 0x4000
      && (high & 0x4000) == 0
      )
    {
      code ^= 0x4000;
      low  |= 0x4000;
    }
  
  exit (0);
}
