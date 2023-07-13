/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Verify that the stack frame is deallocated using the frame pointer.  */

void qq (int a)
{
    char *s = __builtin_alloca (128);
    __builtin_sprintf (s, "qq %d", 3);
}

/* { dg-final { scan-assembler "mov\tr5,sp" } } */
