

#if defined( DEC_INTERN_ASM_CHECK )
#ifdef __DECC
float fasm {
    ... asm stuff ...
};
#pragma intrinsic( dasm )
#endif
/* END ASM TEST*/
#endif  /* DEC_INTERN_ASM_CHECK */
