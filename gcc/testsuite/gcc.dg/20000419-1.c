/* Test for erroneous deletion of the entire macro expansion when pruning
   \r escapes.  Problem noted by DJ Delorie <dj@delorie.com>; test case
   distilled from GNU libc header files.  */
/* { dg-do preprocess } */

#define __REDIRECT(name, proto, alias) name proto __asm__ (__ASMNAME (#alias))
#define __ASMNAME(cname)  __ASMNAME2 (__USER_LABEL_PREFIX__, cname)
#define __ASMNAME2(prefix, cname) __STRING (prefix) cname
#define __STRING(x)    #x

__REDIRECT (a, b, c)
__ASMNAME2 (__USER_LABEL_PREFIX__, harumph)

/* { dg-bogus "used without args" "no args, 1" { target native } 11 } */
/* { dg-bogus "used without args" "no args, 1" { target native } 12 } */
