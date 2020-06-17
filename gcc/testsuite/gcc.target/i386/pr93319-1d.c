/* { dg-do assemble { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target tls_native } */
/* { dg-options "-mx32 -fPIC -mtls-dialect=gnu2 -maddress-mode=long" } */

#include "pr93319-1a.c"
