/* PR target/16952 */
/* { dg-do compile { target { powerpc*-*-linux* && ilp32 } } } */
/* { dg-options "-meabi -mrelocatable" } */
char *s = "boo";
