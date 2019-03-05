/* PR c/71512 */
/* { dg-do compile } */
/* { dg-options "-O -fexceptions -fnon-call-exceptions -ftrapv -fsanitize=undefined" } */
/* { dg-require-effective-target exceptions } */

#include "../../gcc.dg/pr47086.c"
