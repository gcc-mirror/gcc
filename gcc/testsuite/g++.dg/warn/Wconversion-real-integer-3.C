// { dg-do compile }
// { dg-options "-Wconversion -ftrack-macro-expansion=2" }
// { dg-require-effective-target int32plus }

#include "conversion-real-integer-3.h"

float  vfloat;

void h (void)
{
    // We want to trigger an error on the token INT_MAX below, that is
    // a macro that expands to the built-in __INT_MAX__.  Furthermore,
    // INT_MAX is defined inside a system header.
    //
    // The behavior we want is that the diagnostic should point to
    // the locus that inside the source code here, at the relevant
    // line below, even with -ftrack-macro-expansion.  We don't want
    // it to point to the any locus that is inside the system header.
    vfloat = INT_MAX; // { dg-warning "conversion to .float. alters .int. constant value" }
}
