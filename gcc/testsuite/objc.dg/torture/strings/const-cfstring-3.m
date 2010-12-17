/* Test for assigning compile-time constant-string objects to static variables.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>  */

/* So far, CFString is darwin-only.  */
/* { dg-do run { target *-*-darwin* } } */
/* { dg-skip-if "NeXT only" { *-*-* } { "-fgnu-runtime" } { "" } } */
/* { dg-options "-mconstant-cfstrings -framework Foundation" } */

#include <stdlib.h>

typedef const struct __CFString * CFStringRef;
static CFStringRef appKey = (CFStringRef) @"com.apple.soundpref";

static int CFPreferencesSynchronize (CFStringRef ref) {
  return ref == appKey;
}

static void PrefsSynchronize()
{
    if(!CFPreferencesSynchronize(appKey))
      abort();
}

int main(void) {
  PrefsSynchronize();
  return 0;
}
