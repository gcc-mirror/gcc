/* Definitions of target machine for GNU compiler.  HP-UX 68010 version.
   Use this file if GCC is supposed to work with the GNU assembler,
   GNU linker and GNU debugger using DBX debugging information.
   (In other words, much of HPUX has been cast aside.)  */

/* This wants DBX format.  */

#define DBX_DEBUGGING_INFO

#define USE_GAS

#include "m68k/hp310.h"
