/* Include by avx512bw-pr92686-movcc-2.c, avx512vl-pr92686-movcc-2.c  */
__attribute__((noipa)) char
cmpb (char dst, char src1, char src2, int num)
{
  switch(num)
    {
    case 0:
      return src1 == src2 ? src1 : dst;
    case 1:
      return src1 < src2 ? src1 : dst;
    case 2:
      return src1 <= src2 ? src1 : dst;
    case 4:
      return src1 != src2 ? src1 : dst;
    case 5:
      return src1 >= src2 ? src1 : dst;
    case 6:
      return src1 > src2 ? src1 : dst;
    default:
      abort();
    }
  abort();
}

__attribute__((noipa)) unsigned char
cmpub (unsigned char dst, unsigned char src1,
		       unsigned char src2, int num)
{
  switch(num)
    {
    case 0:
      return src1 == src2 ? src1 : dst;
    case 1:
      return src1 < src2 ? src1 : dst;
    case 2:
      return src1 <= src2 ? src1 : dst;
    case 4:
      return src1 != src2 ? src1 : dst;
    case 5:
      return src1 >= src2 ? src1 : dst;
    case 6:
      return src1 > src2 ? src1 : dst;
    default:
      abort();
    }
  abort();
}

__attribute__((noipa)) short
cmpw (short dst, short src1, short src2, int num)
{
  switch(num)
    {
    case 0:
      return src1 == src2 ? src1 : dst;
    case 1:
      return src1 < src2 ? src1 : dst;
    case 2:
      return src1 <= src2 ? src1 : dst;
    case 4:
      return src1 != src2 ? src1 : dst;
    case 5:
      return src1 >= src2 ? src1 : dst;
    case 6:
      return src1 > src2 ? src1 : dst;
    default:
      abort();
    }
  abort();
}

__attribute__((noipa)) unsigned short
cmpuw (unsigned short dst, unsigned short src1,
		       unsigned short src2, int num)
{
  switch(num)
    {
    case 0:
      return src1 == src2 ? src1 : dst;
    case 1:
      return src1 < src2 ? src1 : dst;
    case 2:
      return src1 <= src2 ? src1 : dst;
    case 4:
      return src1 != src2 ? src1 : dst;
    case 5:
      return src1 >= src2 ? src1 : dst;
    case 6:
      return src1 > src2 ? src1 : dst;
    default:
      abort();
    }
  abort();
}

__attribute__((noipa)) int
cmpd (int dst, int src1, int src2, int num)
{
  switch(num)
    {
    case 0:
      return src1 == src2 ? src1 : dst;
    case 1:
      return src1 < src2 ? src1 : dst;
    case 2:
      return src1 <= src2 ? src1 : dst;
    case 4:
      return src1 != src2 ? src1 : dst;
    case 5:
      return src1 >= src2 ? src1 : dst;
    case 6:
      return src1 > src2 ? src1 : dst;
    default:
      abort();
    }
  abort();
}

__attribute__((noipa)) unsigned int
cmpud (unsigned int dst, unsigned int src1,
		       unsigned int src2, int num)
{
  switch(num)
    {
    case 0:
      return src1 == src2 ? src1 : dst;
    case 1:
      return src1 < src2 ? src1 : dst;
    case 2:
      return src1 <= src2 ? src1 : dst;
    case 4:
      return src1 != src2 ? src1 : dst;
    case 5:
      return src1 >= src2 ? src1 : dst;
    case 6:
      return src1 > src2 ? src1 : dst;
    default:
      abort();
    }
  abort();
}

__attribute__((noipa)) long long int
cmpq (long long int dst, long long int src1,
		       long long int src2, int num)
{
  switch(num)
    {
    case 0:
      return src1 == src2 ? src1 : dst;
    case 1:
      return src1 < src2 ? src1 : dst;
    case 2:
      return src1 <= src2 ? src1 : dst;
    case 4:
      return src1 != src2 ? src1 : dst;
    case 5:
      return src1 >= src2 ? src1 : dst;
    case 6:
      return src1 > src2 ? src1 : dst;
    default:
      abort();
    }
  abort();
}

__attribute__((noipa)) unsigned long long int
cmpuq (unsigned long long int dst, unsigned long long int src1,
       unsigned long long int src2, int num)
{
  switch(num)
    {
    case 0:
      return src1 == src2 ? src1 : dst;
    case 1:
      return src1 < src2 ? src1 : dst;
    case 2:
      return src1 <= src2 ? src1 : dst;
    case 4:
      return src1 != src2 ? src1 : dst;
    case 5:
      return src1 >= src2 ? src1 : dst;
    case 6:
      return src1 > src2 ? src1 : dst;
    default:
      abort();
    }
  abort();
}
