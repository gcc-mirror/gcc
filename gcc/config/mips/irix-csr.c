#if _MIPS_SIM == _ABIN32 || _MIPS_SIM == _ABI64
#include <sys/fpu.h>

/* n32 and n64 applications usually run with the MIPS IV Flush to Zero
   bit set.  Clear it here so that gcc-generated code will handle
   subnormals correctly by default.  */

static void __attribute__((constructor))
clear_flush_to_zero (void)
{
  union fpc_csr csr;

  csr.fc_word = get_fpc_csr ();
  csr.fc_struct.flush = 0;
  set_fpc_csr (csr.fc_word);
}
#endif
