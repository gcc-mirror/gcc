/* This caused cc1 to segfault on s390x-ibm-linux
   due to a bug in if_then_else_cond (combine.c).  */

/* { dg-do compile } */
/* { dg-options "-O1" } */

extern void use (unsigned int x);

int main (void)
{
  union 
    {
      unsigned int x;
      unsigned long pad;
    } A;

  struct 
    {
      unsigned int x : 1;
    } B;

  A.x = 1;
  B.x = 1;
  A.x /= B.x;
  use (A.x);

  A.x = 1;
  B.x = 1;
  B.x /= A.x;
  use (B.x);

  return 0;
}

