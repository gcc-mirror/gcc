int main()
{
  int b = 0;

  struct {
    unsigned int bit0:1;
    unsigned int bit1:1;
    unsigned int bit2:1;
    unsigned int bit3:1;
    unsigned int bit4:1;
    unsigned int bit5:1;
    unsigned int bit6:1;
    unsigned int bit7:1;
  } sdata = {0x01};

  while ( sdata.bit0-- > 0 ) {
    b++ ;
    if ( b > 100 ) break;
  }

  if (b != 1)
    abort ();
  return 0;
}

