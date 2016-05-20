/* { dg-do compile } */

int __attribute__ ((common)) common;
static int local;
extern int external_decl;
int external_defn;

int foo ()
{
  return common +  local + external_decl + external_defn;
}

void bar (int i)
{
  common = local = external_decl = external_defn = i;
}

/* { dg-final { scan-assembler "\[\n\r\]\[\t \]*.weak .global\[^,\n\r\]*common" } } */
/* { dg-final { scan-assembler "\[\n\r\]\[\t \]*.global\[^,\n\r\]*local" } } */
/* { dg-final { scan-assembler "\[\n\r\]\[\t \]*.extern .global\[^,\n\r\]*external_decl" } } */
/* { dg-final { scan-assembler "\[\n\r\]\[\t \]*.visible .global\[^,\n\r\]*external_defn" } } */
