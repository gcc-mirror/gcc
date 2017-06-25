/* Test to make sure that stabs for C symbols that go into .comm have the
   proper structure.  These should be lettered G for the struct that gives
   the name to the .comm, and should be V or S for .lcomm symbols.  */

__attribute__ ((used))
static char i_outer;
struct {
   char f1;
   char f2;
} opta;
struct {
   char f1;
   char f2;
} optb;

int
main()
{
   static char i_inner[2] __attribute__ ((used));
   i_inner[0] = 'a'; i_inner[1] = 'b';
   opta.f1 = 'c';
   opta.f2 = 'd';
   optb.f1 = 'C';
   optb.f2 = 'D';
   i_outer = 'e';
/* { dg-do compile } */
/* { dg-skip-if "No stabs" { mmix-*-* alpha*-*-* hppa*64*-*-* ia64-*-* *-*-vxworks* } } */
/* { dg-skip-if "stabs only" { *-*-* } { "*" } { "-gstabs" } } */
   return 0;
}

/* { dg-final { scan-assembler ".stabs.*i_inner:V" } } */
/* { dg-final { scan-assembler ".stabs.*i_outer:S" } } */
/* { dg-final { scan-assembler ".stabs.*opta:G" } } */
/* { dg-final { scan-assembler ".stabs.*optb:G" } } */
