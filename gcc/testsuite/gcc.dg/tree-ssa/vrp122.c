// { dg-do compile }
// { dg-options "-O2 -fdump-tree-ccp1-details" }

void gg(void);
int f(unsigned t)
{
  unsigned g = t*16;
  if (g==0)  return 1;
  gg();
  gg();
  gg();
  gg();
  gg();
  gg();
  if (g<=4)  return 1;
  return 0;
}

// { dg-final { scan-tree-dump "Global Exported: g_.*MASK.*0 VALUE 0x0" "ccp1" } }
