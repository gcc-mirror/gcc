/* PR target/108787 */
/* { dg-do run { target int128 } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-O2 -mdejagnu-cpu=power9" } */

#include "../../gcc.dg/pr108787.c"
