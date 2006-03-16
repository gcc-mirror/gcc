/* PR middle-end/21781.  */
/* { dg-do compile } */

int f[.0e200000000 == 0?1:-1];
