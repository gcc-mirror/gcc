/* { dg-do compile } */
/* { dg-skip-if "" { pdp11-*-* } { "*" } { "" } } */

f(){asm("%0"::"r"(1.5F));}g(){asm("%0"::"r"(1.5));}
