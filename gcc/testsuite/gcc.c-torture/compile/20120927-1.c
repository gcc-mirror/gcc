void ff(int);
int isc_fsaccess_set(unsigned int access) {
 unsigned int mode;
 unsigned int bits;
 mode = 0;
 bits = 0x00000021;
 if ((access & bits) != 0) 
  {
   mode |= 0400;
   access &= ~bits;
  }
 access &= ~bits; 
 bits <<= (10);
 if ((access & bits) != 0)
   access &= ~bits; 
 bits = 0x00000012;
 if ((access & bits) != 0)
 {
   mode |= 0200; 
   access &= ~bits; 
 }
 mode |= (0200 >> 3);
 access &= ~bits; 
 bits <<= (10);
 if ((access & bits) != 0)
   mode |= ((0200 >> 3) >> 3);
 bits = 0x00000044;
 if ((access & bits) != 0)
 { 
    mode |= 0100;
    access &= ~bits;
 }
 if ((access & bits) != 0)
 {
   mode |= (0100 >> 3);
   access &= ~bits; 
 }; 
 bits <<= (10);
 if ((access & bits) != 0)
   mode |= ((0100 >> 3) >> 3);
 ff(mode) ;
}

