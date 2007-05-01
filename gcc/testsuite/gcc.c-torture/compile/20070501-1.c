 typedef signed int signed16 __attribute__ ((__mode__ (__HI__)));
 typedef unsigned int unsigned16 __attribute__ ((__mode__ (__HI__)));
 typedef signed16 HI;
 typedef unsigned16 UHI;
unsigned short f(int y)
{
  HI tmp_b4;
  tmp_b4 = y;
  UHI opval;
  if (tmp_b4 == -32768)
    opval = 32767;
  else
   opval = -tmp_b4;
 return opval;
}

