/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic" } */

enum ebool : bool { BF, BT };

char d[(enum ebool)0.5 == 1 ? 1 : -1];
char f[(enum ebool)0.0 == 0 ? 1 : -1];
