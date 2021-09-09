/* Spurious uninitialized-variable warnings.  */
/* Disable jump threading, etc to test compiler analysis.  */
/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized -fno-tree-dce -fno-tree-vrp -fno-tree-dominator-opts -ftrivial-auto-var-init=zero" } */

#include "uninit-5.c"
