/* Define if your assembler supports specifying the maximum number
   of bytes to skip when using the GAS .p2align command.  */
#undef HAVE_GAS_MAX_SKIP_P2ALIGN

/* Define if your assembler supports .balign and .p2align.  */
#undef HAVE_GAS_BALIGN_AND_P2ALIGN

/* Define if your assembler uses the old HImode fild and fist notation.  */
#undef HAVE_GAS_FILDS_FISTS

/* Define to `int' if <sys/types.h> doesn't define.  */
#undef ssize_t

/* Define if cpp should also search $prefix/include.  */
#undef PREFIX_INCLUDE_DIR

@BOTTOM@

/* Bison unconditionally undefines `const' if neither `__STDC__' nor
   __cplusplus are defined.  That's a problem since we use `const' in
   the GCC headers, and the resulting bison code is therefore type
   unsafe.  Thus, we must match the bison behavior here.  */

#ifndef __STDC__
#ifndef __cplusplus
#undef const
#define const
#endif
#endif
