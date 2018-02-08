/* { dg-do compile } */
/* { dg-options "-O2" } */

void f(char *a)
{
  __builtin_strcpy (a, "");
}

/* { dg-final { scan-assembler-not "ldrb" } } */
