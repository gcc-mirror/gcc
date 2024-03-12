/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mapxf -m64" } */

typedef unsigned int u32;
typedef unsigned long long u64;

void constraint_test ()
{
  register u64 *r16 __asm__("%r16");
  register u64 r17 __asm__("%r17");
  u64 *addr = r16;
  
  __asm__ __volatile__ ("test_mapping_g_m %0, %%rax" : : "g" (r16) : "rax");
  __asm__ __volatile__ ("test_mapping_g_r %0, %%rax" : : "g" (r17) : "rax");
  __asm__ __volatile__ ("test_mapping_m %0, %%rax" : : "m" (addr) : "rax");
  __asm__ __volatile__ ("test_mapping_r %0, %%rax" : : "r" (r17) : "rax");
  __asm__ __volatile__ ("test_mapping_rm %0, %%rax" : "=r,m" (r16) : : "rax");
}

/* { dg-final { scan-assembler-not "test_mapping_g_m %r16, %rax" } } */
/* { dg-final { scan-assembler-not "test_mapping_g_r %r17, %rax" } } */
/* { dg-final { scan-assembler-not "test_mapping_m %r16, %rax" } } */
/* { dg-final { scan-assembler-not "test_mapping_r %r17, %rax" } } */
/* { dg-final { scan-assembler-not "test_mapping_rm %r16, %rax" } } */

