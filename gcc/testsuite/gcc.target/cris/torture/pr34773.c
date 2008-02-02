/* { dg-do run } */
union double_union
{
  double d;
  int i[2];
};
void _dtoa_r (double) __attribute__ ((__noinline__));
void _vfprintf_r (double) __attribute__ ((__noinline__));
void
__sprint_r(int);
void
_vfprintf_r(double da)
{
 double ffp = da;
 double value = ffp;
 union double_union tmp;

 tmp.d = value;

 if ((tmp.i[1]) & ((unsigned)0x80000000L)) {
   value = -value;
 }

 _dtoa_r (value);

 if (ffp != 0)
   __sprint_r(value == 0);
 __asm__ ("");
}


double dd = -.012;
double ff = .012;

void exit (int) __attribute__ ((__noreturn__));
void abort (void) __attribute__ ((__noreturn__));
void *memset(void *s, int c, __SIZE_TYPE__ n);
void _dtoa_r (double d)
{
  if (d != ff)
    abort ();
  __asm__ ("");
}

void __sprint_r (int i)
{
  if (i != 0)
    abort ();
  __asm__ ("");
}

int clearstack (void) __attribute__ ((__noinline__));
int clearstack (void)
{
  char doodle[128];
  memset (doodle, 0, sizeof doodle);
  __asm__ volatile ("" : : "g" (doodle) : "memory");
  return doodle[127];
}

void doit (void) __attribute__ ((__noinline__));
void doit (void)
{
  _vfprintf_r (dd);
  _vfprintf_r (ff);
  __asm__ ("");
}

int main(void)
{
  clearstack ();
  doit ();
  exit (0);
}
