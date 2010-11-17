/* { dg-do compile } */
/* { dg-options "-std=gnu89" } // suppress default -pedantic-errors */

typedef int x, y;
x y z;			/* { dg-error "" "" } */
