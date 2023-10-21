/* { dg-do compile } */
/* { dg-additional-options "-std=gnu89" } */

f(){asm("f":::"cc");}g(x,y){asm("g"::"%r"(x), "r"(y));}
