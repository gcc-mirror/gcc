/* Definitions of target machine for GNU compiler.  HP-UX 68010 version.  */

/* See m68k.h.  0 means 68000 without 68881 and no bitfields.  */
#define	TARGET_DEFAULT 0

#include "m68k/hp320.h"

/* Don't try using XFmode.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64

#undef CPP_SPEC
#undef ASM_SPEC

/* HP does not support a 68020 without a 68881 or a 68010 with a 68881.
   However, -m68020 does not imply -m68881.  You must specify both
   if you want both.  */

#ifdef HPUX_ASM

#define CPP_SPEC "-D__HPUX_ASM__ %{m68881: -D__HAVE_68881__}\
%{m68020: -Dmc68020}%{mc68020: -Dmc68020}\
%{!traditional:-D_INCLUDE__STDC__}"

#define ASM_SPEC "%{!m68020:%{!mc68020:+X}}"

#else	/* not HPUX_ASM */

#define CPP_SPEC "%{m68881: -D__HAVE_68881__}\
%{m68020: -Dmc68020}%{mc68020: -Dmc68020}\
%{!traditional:-D_INCLUDE__STDC__}"

#define ASM_SPEC \
 "%{m68000:-mc68000}%{mc68000:-mc68000}%{!mc68000:%{!m68000:-mc68020}}"

#endif	/* not HPUX_ASM */
