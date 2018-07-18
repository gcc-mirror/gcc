/* { dg-do compile { target { ! default_mode } } } */
/* { dg-skip-if "-marm/-mthumb/-march/-mcpu given" { *-*-* } { "-marm" "-mthumb" "-march=*" "-mcpu=*" } } */
/* { dg-options "-mcpu=cortex-m4" } */

/* Check that -mthumb is not needed when compiling for a Thumb-only target.  */

int foo;
