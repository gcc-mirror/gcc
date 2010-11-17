/* { dg-do compile } */
/* { dg-options "-std=gnu89" } // suppress default -pedantic-errors */

typedef int x, y;
x struct f z; /* { dg-error "two or more " "" } */
