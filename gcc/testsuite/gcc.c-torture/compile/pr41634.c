extern int _xgetw();
extern int foo(char*);

void test_readmode( int ascii_mode )
{
  static const char outbuffer[]
    = "0,1,2,3,4,5,6,7,8,9\r\n\r\nA,B,C,D,E\r\nX,Y,Z";
  char buffer[2*512 +256];
  int i, j, ao;
  unsigned int fp;

  foo(buffer);

  for (i=0, j=0; i<6; i++) {
      if (ao==0 || outbuffer[fp-3+i] != '\r')
	buffer[j++] = outbuffer[fp-3+i];
  }
  _xgetw();
}
