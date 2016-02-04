/* PR target/69072 */
/* Reported by Zdenek Sojka <zsojka@seznam.cz> */

/* { dg-do compile } */

typedef struct
{
  struct
  {
    double d;
  } __attribute__((packed)) a;
} S;

void
foo (S s1, S s2, S s3, S s4, S s5, S s6, S s7)
{}
