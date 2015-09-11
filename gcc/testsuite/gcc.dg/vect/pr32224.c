/* { dg-do compile } */
typedef unsigned long int *mp_ptr;
typedef const unsigned long int *mp_srcptr;
void
gmpz_export (void *data)
{
  mp_srcptr zp;
  int count, i;
  mp_ptr __dst = ((mp_ptr) data);
  mp_srcptr __src = (zp);

  for (i = 0; i < count; i++)
  {
    __asm__ ("checkme": "=r" (*__dst):"0" (*(__src)));
    __src++;
  }
}
