/* { dg-do run } */
/* { dg-options "-save-temps -O2 -fno-omit-frame-pointer -mtune-ctrl=prologue_using_move,epilogue_using_move,use_leave" } */

#define DONT_SAVE_REGS1 __attribute__((preserve_none))
#define DONT_SAVE_REGS2 __attribute__((preserve_none))

/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-linux*" } {^\t?\.}  } } */

/*
**do_test:
**.LFB[0-9]+:
**...
**	leave
**...
**	ret
**...
*/

#include "pr120840-1a.c"
