/* Like iris4.h, but always inhibits assembler optimization.
   Use this via mips-sgi-iris4loser if you need it.  */

#include "mips/iris4.h"

#undef ASM_SPEC
#define ASM_SPEC "\
%{!mgas: \
	%{!mrnames: %{!.s:-nocpp} %{.s: %{cpp} %{nocpp}}} \
	%{pipe: %e-pipe is not supported.} \
	%{mips1} %{mips2} %{mips3} \
	-O0 \
	%{g} %{g0} %{g1} %{g2} %{g3} %{v} %{K} \
	%{ggdb:-g} %{ggdb0:-g0} %{ggdb1:-g1} %{ggdb2:-g2} %{ggdb3:-g3} \
	%{gstabs:-g} %{gstabs0:-g0} %{gstabs1:-g1} %{gstabs2:-g2} %{gstabs3:-g3} \
	%{gstabs+:-g} %{gstabs+0:-g0} %{gstabs+1:-g1} %{gstabs+2:-g2} %{gstabs+3:-g3} \
	%{gcoff:-g} %{gstabs0:-g0} %{gcoff1:-g1} %{gcoff2:-g2} %{gcoff3:-g3}} \
%{G*}"
