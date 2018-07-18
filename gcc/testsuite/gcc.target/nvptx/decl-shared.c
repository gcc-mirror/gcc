static int v_internal __attribute__((shared,used));
int v_common __attribute__((shared,common));
int v_extdef __attribute__((shared,nocommon));
extern int v_extdecl __attribute__((shared));

int use()
{
  return v_extdecl;
}

/* { dg-final { scan-assembler "\[\r\n\]\[\t \]*.shared \[^,\r\n\]*v_internal" } } */
/* { dg-final { scan-assembler "\[\r\n\]\[\t \]*.weak .shared \[^,\r\n\]*v_common" } } */
/* { dg-final { scan-assembler "\[\r\n\]\[\t \]*.visible .shared \[^,\r\n\]*v_extdef" } } */
/* { dg-final { scan-assembler "\[\r\n\]\[\t \]*.extern .shared \[^,\r\n\]*v_extdecl" } } */
