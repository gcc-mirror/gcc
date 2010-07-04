/* -O as an operand to another option should not take effect as an
    optimization option.  */
/* { dg-do compile } */
/* { dg-options "-I -O" } */

#ifdef __OPTIMIZE__
#error "__OPTIMIZE__ defined"
#endif
