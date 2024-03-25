/* { dg-do compile } */
/* { dg-options "-fstrub=strict -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

#include <stdarg.h>

void __attribute__ ((__strub__ ("internal")))
small_args (int i, long long l, void *p, void **q, double d, char c)
{
}

/* { dg-final { scan-ipa-dump "\n(void )?\[^ \]*small_args\[^ \]*.strub.\[0-9\]* \[(\]int i, long long int l, void \\* p, void \\* \\* q, double d, char c, void \\* &\[^&,\]*.strub.watermark_ptr\[)\]" "strub" } } */
/* { dg-final { scan-ipa-dump " \[^ \]*small_args\[^ \]*.strub.\[0-9\]* \[(\]\[^&\]*&.strub.watermark.\[0-9\]*\[)\]" "strub" } } */


struct large_arg {
  int x[128];
};

void __attribute__ ((__strub__ ("internal")))
large_byref_arg (struct large_arg la)
{
}

/* { dg-final { scan-ipa-dump "\n(void )?\[^ \]*large_byref_arg\[^ \]*.strub.\[0-9\]* \[(\]struct large_arg & la, void \\* &\[^&,\]*.strub.watermark_ptr\[)\]" "strub" } } */
/* { dg-final { scan-ipa-dump " \[^ \]*large_byref_arg\[^ \]*.strub.\[0-9\]* \[(\]&\[^&\]*&.strub.watermark.\[0-9\]*\[)\]" "strub" } } */

void __attribute__ ((__strub__ ("internal")))
std_arg (int i, ...)
{
  va_list vl;
  va_start (vl, i);
  va_end (vl);
}

/* { dg-final { scan-ipa-dump "\n(void )?\[^ \]*std_arg\[^ \]*.strub.\[0-9\]* \[(\]int i, \[^&,\]* &\[^&,\]*.strub.va_list_ptr, void \\* &\[^&,\]*.strub.watermark_ptr\[)\]" "strub" } } */
/* { dg-final { scan-ipa-dump " \[^ \]*std_arg\[^ \]*.strub.\[0-9\]* \[(\]\[^&\]*&.strub.va_list.\[0-9\]*, &.strub.watermark.\[0-9\]*\[)\]" "strub" } } */
/* { dg-final { scan-ipa-dump-times "va_start \\(" 1 "strub" } } */
/* { dg-final { scan-ipa-dump-times "va_copy \\(" 1 "strub" } } */
/* { dg-final { scan-ipa-dump-times "va_end \\(" 2 "strub" } } */

void __attribute__ ((__strub__ ("internal")))
apply_args (int i, int j, double d)
{
  __builtin_apply_args ();
}

/* { dg-final { scan-ipa-dump "\n(void )?\[^ \]*apply_args\[^ \]*.strub.\[0-9\]* \[(\]int i, int j, double d, void \\*\[^&,\]*.strub.apply_args, void \\* &\[^&,\]*.strub.watermark_ptr\[)\]" "strub" } } */
/* { dg-final { scan-ipa-dump " \[^ \]*apply_args\[^ \]*.strub.\[0-9\]* \[(\]\[^&\]*.strub.apply_args.\[0-9\]*_\[0-9\]*, &.strub.watermark.\[0-9\]*\[)\]" "strub" } } */
