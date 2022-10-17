/* { dg-do compile }
/* { dg-options "-Wconversion -ftrack-macro-expansion=2" } */
/* { dg-require-effective-target int32plus } */

// Before the fix that came with this test, we'd output an error for
// the __INT_MAX__ token.  That token has a BUILTINS_LOCATION
// location, so the location prefix in the warning message would
// be:
//     <built-in>:0:0: warning: conversion to 'float' alters 'int' constant value
//
// Note the useless and confusing <built-in>:0:0 prefix.  This is
// because '__INT_MAX__' being an internal macro token, it has a
// BUILTINS_LOCATION location.
//
// In this case, we want the error message to refer to the first
// location (in the macro expansion context) that is not a location
// for a built-in token.  That location would be the one for where (in
// real source code) the __INT_MAX__ macro has been expanded.
//
// That would be something like:
//
//     gcc/testsuite/g++.dg/warn/Wconversion-real-integer2.C:21:17: warning: conversion to 'float' alters 'int' constant value
//
// That is more useful.

#define INT_MAX __INT_MAX__ // { dg-warning "17: conversion from 'int' to 'float' changes value from .2147483647. to " }

float  vfloat;

void h (void)
{
    vfloat = INT_MAX; // { dg-message "14: in expansion of macro .INT_MAX." }
}
