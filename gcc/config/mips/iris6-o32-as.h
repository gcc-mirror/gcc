/* Definitions of target machine for GNU compiler, for MIPS running IRIX 6
   (O32 ABI) using the SGI assembler.  */

/* Override mips.h default: the IRIX 6 O32 assembler warns about -O3:

   as: Warning: -O3 is not supported for assembly compiles for ucode
   compilers; changing to -O2.
   
   So avoid passing it in the first place.  */
#undef SUBTARGET_ASM_OPTIMIZING_SPEC
#define SUBTARGET_ASM_OPTIMIZING_SPEC "\
%{noasmopt:-O0} \
%{!noasmopt:%{O|O1|O2|O3:-O2}}"
