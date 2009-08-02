/* If we AND in DI mode (i.e. replace the order of TRUNCATE and AND) then we
   can remove the TRUNCATE.  */
/* { dg-options "-O -mgp64" } */
/* { dg-final { scan-assembler-not "\tsll\t\[^\n\]*,0" } } */

struct s
{
  unsigned a:5;
};

NOMIPS16 void
f (struct s *s, unsigned long long a)
{
  s->a = a & 0x3;
}
