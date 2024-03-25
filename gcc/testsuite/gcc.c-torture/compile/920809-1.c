/* { dg-additional-options "-std=gnu89" } */

f(x,y){memcpy (&x,&y,8192);}
