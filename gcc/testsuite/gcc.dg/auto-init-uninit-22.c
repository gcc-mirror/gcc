/* { dg-do compile } */
/* { dg-options "-O3 -Wuninitialized --param vect-max-version-for-alias-checks=20 -ftrivial-auto-var-init=zero" } */
#include "uninit-22.c"
