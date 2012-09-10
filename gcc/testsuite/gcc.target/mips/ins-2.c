/* { dg-do compile } */
/* { dg-options "-meb isa_rev>=2 -mgp64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-times "\tins\t|\tdins\t" 1 } } */
/* { dg-final { scan-assembler-times "\tsll\t|\tins\t" 1 } } */

/* When inserting something into the top bit of a 32-bit structure,
   we must make sure that the register remains properly sign-extended.
   There are two ways of doing this:
 
     - use purely 32-bit bit manipulations (a single INS, matched twice here).
     - use a 64-bit bit manipulation (DINS), and sign-extend the result.  We
     check for this extension using SLL.  */

struct s
{
  int a:3;
  int b:29;
};

NOMIPS16 void
f (int a)
{
  struct s s;
  asm volatile ("" : "=r"(s));
  s.a = a;
  asm volatile ("" :: "r"(s));
}
