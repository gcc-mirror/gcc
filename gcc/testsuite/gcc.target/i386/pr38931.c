/* { dg-do compile } */
/* { dg-options "-O2 -msse" } */

typedef int __m64 __attribute__ ((__vector_size__ (8)));

extern __m64 foo () ;

void bar (const int input_bpl, const unsigned char *input,
	  unsigned char *output, unsigned long x1)
{
  unsigned char *pix_end_ptr = output + x1 * 4;
  __m64 m_original = { 0, 0 };
  __m64 m_base_addr = __builtin_ia32_vec_init_v2si (0, input_bpl);
  __m64 m_addr = __builtin_ia32_paddd (m_original, m_base_addr);
  __m64 *a0 = (__m64 *) input;

  for (; output < pix_end_ptr; output += 4)
    {
      a0 = (__m64 *) (input + __builtin_ia32_vec_ext_v2si (m_addr, 0));
      m_addr = foo ();
      __builtin_prefetch (a0, 0);
    }
}
