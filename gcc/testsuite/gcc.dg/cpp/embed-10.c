/* This is a comment with some UTF-8 non-ASCII characters: áéíóú.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -Wconversion" } */

signed char a[] = {
#embed __FILE__	/* { dg-warning "conversion from 'int' to 'signed char' changes value from '\[12]\[0-9]\[0-9]' to '-\[0-9]\[0-9]*'" } */
};
