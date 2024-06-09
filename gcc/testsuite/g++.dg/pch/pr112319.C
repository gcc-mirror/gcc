/* { dg-additional-options "-Wpragmas -save-temps" } */
#include "pr112319.H"
#pragma GCC diagnostic error "-Wpragmas"
#pragma GCC diagnostic ignored "oops" /* { dg-error "oops" } */
/* { dg-regexp {[^[:space:]]*: some warnings being treated as errors} } */
