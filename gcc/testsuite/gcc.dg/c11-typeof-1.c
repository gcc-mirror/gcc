/* Test typeof and typeof_unqual not keywords in C11.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

int typeof = 1;
long typeof_unqual = 2;
