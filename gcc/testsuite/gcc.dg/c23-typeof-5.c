/* Test C23 typeof and typeof_unqual on arrays of atomic elements (bug
   117781).  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

_Atomic int a[2], b[2][2];
const _Atomic int c[2], d[2][2];

extern typeof (a) a;
extern typeof (b) b;
extern typeof (c) c;
extern typeof (d) d;
extern typeof_unqual (a) a;
extern typeof_unqual (b) b;
extern typeof_unqual (c) a;
extern typeof_unqual (d) b;
extern typeof_unqual (volatile _Atomic int [2]) a;
extern typeof_unqual (volatile _Atomic int [2][2]) b;
