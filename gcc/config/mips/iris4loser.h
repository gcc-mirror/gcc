/* Like iris4.h, but always inhibits assembler optimization for MIPS as.
   Use this via mips-sgi-iris4loser if you need it.  */

#include "mips/iris4.h"

#undef ASM_SPEC
#if ((TARGET_CPU_DEFAULT | TARGET_DEFAULT) & MASK_GAS) != 0
/* GAS */
#define ASM_SPEC "\
%{noasmopt:-O0} \
%{!noasmopt:%{O:-O2} %{O1:-O2} %{O2:-O2} %{O3:-O3}} \
%{mmips-as: \
	%{!.s:-nocpp} %{.s: %{cpp} %{nocpp}} \
	-O0 \
	%{pipe: %e-pipe is not supported.} \
	%{K}} \
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} %{v} \
%{g} %{g0} %{g1} %{g2} %{g3} \
%{ggdb:-g} %{ggdb0:-g0} %{ggdb1:-g1} %{ggdb2:-g2} %{ggdb3:-g3} \
%{gstabs:-g} %{gstabs0:-g0} %{gstabs1:-g1} %{gstabs2:-g2} %{gstabs3:-g3} \
%{gstabs+:-g} %{gstabs+0:-g0} %{gstabs+1:-g1} %{gstabs+2:-g2} %{gstabs+3:-g3} \
%{gcoff:-g} %{gcoff0:-g0} %{gcoff1:-g1} %{gcoff2:-g2} %{gcoff3:-g3}"

#else
/* not GAS */
#define ASM_SPEC "\
%{noasmopt:-O0} \
%{!noasmopt:%{O:-O2} %{O1:-O2} %{O2:-O2} %{O3:-O3}} \
%{!mgas: \
	%{!.s:-nocpp} %{.s: %{cpp} %{nocpp}} \
	-O0 \
	%{pipe: %e-pipe is not supported.} \
	%{K}} \
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} %{v} \
%{g} %{g0} %{g1} %{g2} %{g3} \
%{ggdb:-g} %{ggdb0:-g0} %{ggdb1:-g1} %{ggdb2:-g2} %{ggdb3:-g3} \
%{gstabs:-g} %{gstabs0:-g0} %{gstabs1:-g1} %{gstabs2:-g2} %{gstabs3:-g3} \
%{gstabs+:-g} %{gstabs+0:-g0} %{gstabs+1:-g1} %{gstabs+2:-g2} %{gstabs+3:-g3} \
%{gcoff:-g} %{gcoff0:-g0} %{gcoff1:-g1} %{gcoff2:-g2} %{gcoff3:-g3}"

#endif
