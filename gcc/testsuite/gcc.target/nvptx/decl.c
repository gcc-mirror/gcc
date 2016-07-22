/* { dg-do compile } */

static const int __attribute__ ((used)) cst_local = 4;
static int __attribute__ ((used)) glob_local = 5;
const int __attribute__ ((used)) cst_export = 4;
int __attribute__ ((used)) glob_export = 5;
extern const int cst_import;
extern int glob_import;

int Foo ()
{
  return cst_import + glob_import;
}

/* { dg-final { scan-assembler "\[\r\n\]\[\t \]*.visible .global \[^,\r\n\]*glob_export" } } */
/* { dg-final { scan-assembler "\[\r\n\]\[\t \]*.visible .const \[^,\r\n\]*cst_export" } } */
/* { dg-final { scan-assembler "\[\r\n\]\[\t \]*.global \[^,\r\n\]*glob_local" } } */
/* { dg-final { scan-assembler "\[\r\n\]\[\t \]*.const \[^,\r\n\]*cst_local" } } */
/* { dg-final { scan-assembler "\[\r\n\]\[\t \]*.extern .global \[^,\r\n\]*glob_import" } } */
/* { dg-final { scan-assembler "\[\r\n\]\[\t \]*.extern .const \[^,\r\n\]*cst_import" } } */
