/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic" } */

enum e { A = (_Bool) 0.0, B = (_Bool) 0.5, C = (_Bool) 1.0 };
