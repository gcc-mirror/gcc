/* { dg-do compile } */
/* { dg-require-effective-target named_sections } */
/* { dg-final { scan-assembler "mysection" } } */
extern char foo;
char foo __attribute__ ((__section__(".mysection")));
