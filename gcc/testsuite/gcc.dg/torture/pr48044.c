/* PR middle-end/48044 */
/* { dg-do compile } */
/* { dg-require-alias "" } */

#define ASMNAME(cname)  ASMNAME2 (__USER_LABEL_PREFIX__, cname)
#define ASMNAME2(prefix, cname) STRING (prefix) cname
#define STRING(x)    #x

int a __asm__ (ASMNAME ("b")) = 0;
extern int c __asm__ (ASMNAME ("a")) __attribute__ ((alias ("b")));
extern int d __attribute__ ((weak, alias ("a")));
