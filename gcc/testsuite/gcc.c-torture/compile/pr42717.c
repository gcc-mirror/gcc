/* { dg-xfail-if "ptxas crashes" { nvptx-*-* } { "-O0" } { "" } } */
static signed char
foo (signed char si1, unsigned char si2)
{
  return (si1 ^ si2) & (-si2 ^ si2) ? : si1 - si2;
}

struct S0
{
};

unsigned char g_21;

struct S0 g_34;

void
bar (unsigned char p_20)
{
  unsigned char *l_22 = &g_21;
  unsigned char l_23 = 0;
  struct S0 *l = &g_34;
  goto lbl_42;
  for (; l_23; l_23 = foo (l_23, 1))
    {
      for (p_20 = 0; 0; p_20 = foo (p_20, 1))
	lbl_42:;
      (l == &g_34) ? 0 : "";
lbl_85:*l_22 = p_20;
    }
  goto lbl_85;
}
