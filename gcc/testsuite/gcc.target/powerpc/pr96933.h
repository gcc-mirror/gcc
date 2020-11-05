/* Source file for pr96933-*.c testing, this mainly contains 4
   functions as below:

     - test_uchar  // vector unsigned char
     - test_schar  // vector signed char
     - test_ushort // vector unsigned short
     - test_sshort // vector signed short
*/

__attribute__ ((noipa)) vector unsigned char
test_uchar (unsigned char c1, unsigned char c2, unsigned char c3,
	    unsigned char c4, unsigned char c5, unsigned char c6,
	    unsigned char c7, unsigned char c8, unsigned char c9,
	    unsigned char c10, unsigned char c11, unsigned char c12,
	    unsigned char c13, unsigned char c14, unsigned char c15,
	    unsigned char c16)
{
  vector unsigned char v
    = {c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16};
  return v;
}

__attribute__ ((noipa)) vector signed char
test_schar (signed char c1, signed char c2, signed char c3, signed char c4,
	    signed char c5, signed char c6, signed char c7, signed char c8,
	    signed char c9, signed char c10, signed char c11, signed char c12,
	    signed char c13, signed char c14, signed char c15, signed char c16)
{
  vector signed char v
    = {c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16};
  return v;
}

__attribute__ ((noipa)) vector unsigned short
test_ushort (unsigned short c1, unsigned short c2, unsigned short c3,
	     unsigned short c4, unsigned short c5, unsigned short c6,
	     unsigned short c7, unsigned short c8)
{
  vector unsigned short v = {c1, c2, c3, c4, c5, c6, c7, c8};
  return v;
}

__attribute__ ((noipa)) vector signed short
test_sshort (signed short c1, signed short c2, signed short c3,
	     signed short c4, signed short c5, signed short c6,
	     signed short c7, signed short c8)
{
  vector signed short v = {c1, c2, c3, c4, c5, c6, c7, c8};
  return v;
}
