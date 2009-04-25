/* Test for enumeration constants not integer constant expressions but
   folding to integer constants (used in Linux kernel,
   <http://gcc.gnu.org/ml/gcc/2009-04/msg00677.html>).  */
/* { dg-do compile } */
/* { dg-options "" } */

extern int i;
enum e { E = (1 ? 1 : i) };
