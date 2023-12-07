/* { dg-do compile } */
/* { dg-options "-fstrub=strict -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

#include <stdarg.h>

void __attribute__ ((__strub__ ("at-calls")))
small_args (int i, long long l, void *p, void **q, double d, char c)
{
}

/* { dg-final { scan-ipa-dump "\n(void )?\[^ \]*small_args\[^ \]* \[(\]int i, long long int l, void \\* p, void \\* \\* q, double d, char c, void \\* &\[^&,\]*.strub.watermark_ptr\[)\]" "strub" } } */


struct large_arg {
  int x[128];
};

void __attribute__ ((__strub__ ("at-calls")))
large_byref_arg (struct large_arg la)
{
}

/* { dg-final { scan-ipa-dump "\n(void )?\[^ \]*large_byref_arg\[^ \]* \[(\]struct large_arg la, void \\* &\[^&,\]*.strub.watermark_ptr\[)\]" "strub" } } */

void __attribute__ ((__strub__ ("at-calls")))
std_arg (int i, ...)
{
  va_list vl;
  va_start (vl, i);
  va_end (vl);
}

/* { dg-final { scan-ipa-dump "\n(void )?\[^ \]*std_arg\[^ \]* \[(\]int i, void \\* &\[^&,\]*.strub.watermark_ptr\[, .]*\[)\]" "strub" } } */
/* { dg-final { scan-ipa-dump-times "va_start \\(" 1 "strub" } } */
/* { dg-final { scan-ipa-dump-not "va_copy \\(" "strub" } } */
/* { dg-final { scan-ipa-dump-times "va_end \\(" 1 "strub" } } */
