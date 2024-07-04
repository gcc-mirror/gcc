/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic" } */

enum ebool : bool { BF, BT };

enum e { A = (enum ebool) 0.0, B = (enum ebool) 0.5, C = (enum ebool) 1.0 };
