/* { dg-do compile } */
/* { dg-options "-finstrument-functions -finstrument-functions-exclude-function-list=fn" } */

void fn () { }

/* { dg-final { scan-assembler-not "__cyg_profile_func_enter" } } */
/* { dg-final { scan-assembler-not "__cyg_profile_func_exit" } } */
