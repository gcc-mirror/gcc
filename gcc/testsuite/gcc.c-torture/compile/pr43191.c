/* { dg-skip-if "Ints are 16 bits" { "pdp11-*-*" } { "*" } { "" } } */ 
struct S0
{
};

struct S1
{
  unsigned f0:27;
  const unsigned:0;
};

struct S2
{
  unsigned f2:1;
};

unsigned char g_4[1][8][3][1][1][1];
unsigned char *g_17;
unsigned char **g_16[1][10][7];

struct S2 g_35 = {
  0
};

struct S2 *g_34 = &g_35;

struct S1 func_86 (unsigned char p_87, struct S2 **p_89)
{
  struct S1 l_92[6][8][1][1] = {
    16143586
  }
  ;
  return l_92[0][0][0][0];
}

void func_28 (struct S1 p_30, const struct S1 p_32)
{
}

void func_70 (unsigned char p_72)
{
  unsigned char *const *l_93 = &g_17;
  struct S2 **l_94;
  unsigned char *const *l_97 = &g_17;
  func_28 (func_86 (p_72, 0),
           func_86 (p_72, &g_34));
}
