/* Check vector shift patterns.  */

/* { dg-options "-march=z13 -O1 -m64" } */

/* { dg-final { scan-assembler "veslb\t%v.+,%v.+,2.%r2" } } */
/* { dg-final { scan-assembler "veslh\t%v.+,%v.+,3.%r2" } } */
/* { dg-final { scan-assembler "veslf\t%v.+,%v.+,4.%r2" } } */
/* { dg-final { scan-assembler "veslg\t%v.+,%v.+,5.%r2" } } */
/* { dg-final { scan-assembler "vesrab\t%v.+,%v.+,2.%r2" } } */
/* { dg-final { scan-assembler "vesrah\t%v.+,%v.+,3.%r2" } } */
/* { dg-final { scan-assembler "vesraf\t%v.+,%v.+,4.%r2" } } */
/* { dg-final { scan-assembler "vesrag\t%v.+,%v.+,5.%r2" } } */
/* { dg-final { scan-assembler "vesrlb\t%v.+,%v.+,2.%r2" } } */
/* { dg-final { scan-assembler "vesrlh\t%v.+,%v.+,3.%r2" } } */
/* { dg-final { scan-assembler "vesrlf\t%v.+,%v.+,4.%r2" } } */
/* { dg-final { scan-assembler "vesrlg\t%v.+,%v.+,5.%r2" } } */
/* { dg-final { scan-assembler-not "ahi" } } */
/* { dg-final { scan-assembler-not "nilf" } } */
/* { dg-final { scan-assembler-not "risbg" } } */

typedef __attribute__((vector_size(16))) signed char v16qi;

v16qi vshiftlqi (v16qi in, unsigned int sh)
{
  sh = (sh + 2) % 8;
  return (in << sh);
}

typedef __attribute__((vector_size(16))) signed short v8hi;

v8hi vshiftlhi (v8hi in, unsigned int sh)
{
  sh = (sh + 3) % 16;
  return (in << sh);
}

typedef __attribute__((vector_size(16))) signed int v4si;

v4si vshiftlsi (v4si in, unsigned int sh)
{
  sh = (sh + 4) % 32;
  return (in << sh);
}

typedef __attribute__((vector_size(16))) signed long v2di;

v2di vshiftldi (v2di in, unsigned int sh)
{
  sh = (sh + 5) % 64;
  return (in << sh);
}

typedef __attribute__((vector_size(16))) unsigned char uv16qi;

uv16qi vshiftrqiu (uv16qi in, unsigned int sh)
{
  sh = (sh + 2) % 8;
  return (in >> sh);
}

typedef __attribute__((vector_size(16))) unsigned short uv8hi;

uv8hi vshiftrhiu (uv8hi in, unsigned int sh)
{
  sh = (sh + 3) % 16;
  return (in >> sh);
}

typedef __attribute__((vector_size(16))) unsigned int uv4si;

uv4si vshiftrsiu (uv4si in, unsigned int sh)
{
  sh = (sh + 4) % 32;
  return (in >> sh);
}

typedef __attribute__((vector_size(16))) unsigned long uv2di;

uv2di vshiftrdiu (uv2di in, unsigned int sh)
{
  sh = (sh + 5) % 64;
  return (in >> sh);
}

v16qi vshiftrqi (v16qi in, unsigned int sh)
{
  sh = (sh + 2) % 8;
  return (in >> sh);
}

v8hi vshiftrhi (v8hi in, unsigned int sh)
{
  sh = (sh + 3) % 16;
  return (in >> sh);
}

v4si vshiftrsi (v4si in, unsigned int sh)
{
  sh = (sh + 4) % 32;
  return (in >> sh);
}

v2di vshiftrdi (v2di in, unsigned int sh)
{
  sh = (sh + 5) % 64;
  return (in >> sh);
}
