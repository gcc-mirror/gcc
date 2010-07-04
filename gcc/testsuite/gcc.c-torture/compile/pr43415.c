int main()                                                                      
{                                                                               
  unsigned long long table[256];                                          
  unsigned int i;
  for (i=0; i<256; ++i) {
      unsigned long long j;
      unsigned char x=i;
      for (j=0; j<5; ++j) {
	  x += x<<1;
	  x ^= x>>1;
      }
      for (j=0; j<5; ++j) {
	  x += x<<1;
	  x ^= x>>1;
      }
      for (j=0; j<5; ++j) {
	  x += x<<1;
	  x ^= x>>1;
      }
      for (j=0; j<5; ++j) {
	  x += x<<1;
	  x ^= x>>1;
      }
      for (j=0; j<5; ++j) {
	  x += x<<1;
	  x ^= x>>1;
      }
      table[i] ^= (((unsigned long long)x)<<16);
  }
  for (i=0; i<256; ++i) {
      if ((table[i]&0xff)==i)
	return 1;
  }
  return 0;
}

