/* { dg-do compile } */
/* { dg-options "-fmerge-all-constants" } */

char *ca = "123";

const char a[] __attribute__((__progmem__))= "a";
const char b[] __attribute__((__progmem__))= "b";
