/* { dg-options "-O2" } */

typedef unsigned long long uint64_t;

static inline void cvmx_write64_uint64(uint64_t addr, uint64_t val)
{
  *(volatile uint64_t *)(long)addr = val;
};
static inline uint64_t cvmx_read64_uint64(uint64_t addr)
{
  return *(volatile uint64_t *)(long)addr;
};
static inline void cvmx_write_csr(uint64_t csr_addr, uint64_t val)
{
    cvmx_write64_uint64 (csr_addr, val);
    if ((csr_addr >> 40) != 0x800118)
      linker_error ();
}

int interface;
int idx;

int
main ()
{
    cvmx_write_csr((((((uint64_t) 2LL) << 62)
		     | ((0x0001180008000208ull
			 + (((idx)&3)*2048)
			 + (((interface)&1)*0x8000000ull))))), 1);
  return 0;
}
