/* { dg-do compile } */
/* { dg-options "-O2" } */

static inline unsigned long
lay_uw(unsigned long addr)
{
  unsigned long result;

  __asm__ ("lay    %[result],%a[addr]"
	   : [result] "=d" (result)
	   : [addr] "UW" (addr));
  return result;
}

static inline unsigned long
la_u(unsigned long addr)
{
  unsigned long result;

  __asm__ ("la    %[result],%a[addr]"
	   : [result] "=d" (result)
	   : [addr] "U" (addr));
  return result;
}

static inline unsigned long
lay_zqzrzszt(unsigned long addr)
{
  unsigned long result;

  __asm__ ("lay    %[result],%a[addr]"
	   : [result] "=d" (result)
	   : [addr] "ZQZRZSZT" (addr));
  return result;
}

static inline unsigned long
la_zqzr(unsigned long addr)
{
  unsigned long result;

  __asm__ ("la    %[result],%a[addr]"
	   : [result] "=d" (result)
	   : [addr] "ZQZR" (addr));
  return result;
}


extern unsigned long a[15];

int main(void)
{
  a[1] = lay_uw(3333);
  a[2] = lay_uw(4444);
  a[3] = lay_uw(1000000);
  a[4] = lay_uw(a[0]);

  a[5] = la_u(2222);
  a[6] = la_u(5555);
  a[7] = la_u(a[0]);

  a[8] = lay_zqzrzszt(3333);
  a[9] = lay_zqzrzszt(4444);
  a[10] = lay_zqzrzszt(1000000);
  a[11] = lay_zqzrzszt(a[0]);

  a[12] = la_zqzr(2222);
  a[13] = la_zqzr(5555);
  a[14] = la_zqzr(a[0]);
}
