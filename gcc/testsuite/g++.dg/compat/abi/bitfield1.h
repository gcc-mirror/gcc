typedef int Int;
typedef signed int SInt;
typedef unsigned int UInt;

struct A
{
  SInt bitS : 1;	// signed
  UInt bitU : 1;	// unsigned
  Int bit : 1;		// signedness by -f{signed,unsigned}-bitfields
};
