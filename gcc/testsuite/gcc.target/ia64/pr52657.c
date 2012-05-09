/* { dg-do compile } */
/* { dg-options "-O" } */

typedef unsigned long int mp_limb_t;

typedef struct
{
  int _mp_alloc;
  int _mp_size;
  mp_limb_t *_mp_d;
} __mpz_struct;

typedef __mpz_struct mpz_t[1];
typedef mp_limb_t * mp_ptr;
typedef const mp_limb_t * mp_srcptr;
typedef long int mp_size_t;

extern mp_limb_t __gmpn_addmul_2 (mp_ptr, mp_srcptr, mp_size_t, mp_srcptr);

void
__gmpn_redc_2 (mp_ptr rp, mp_ptr up, mp_srcptr mp, mp_size_t n, mp_srcptr mip)
{
  mp_limb_t q[2];
  mp_size_t j;
  mp_limb_t upn;

  for (j = n - 2; j >= 0; j -= 2)
    {
      mp_limb_t _ph, _pl;
      __asm__ ("xma.hu %0 = %3, %5, f0\n\t"
               "xma.l %1 = %3, %5, f0\n\t"
               ";;\n\t"
               "xma.l %0 = %3, %4, %0\n\t"
               ";;\n\t"
               "xma.l %0 = %2, %5, %0"
               : "=&f" (q[1]), "=&f" (q[0])
               : "f" (mip[1]), "f" (mip[0]), "f" (up[1]), "f" (up[0]));
      upn = up[n];
      up[1] = __gmpn_addmul_2 (up, mp, n, q);
      up[0] = up[n];
      up[n] = upn;
      up += 2;
    }
}
