/* { dg-do compile } */

typedef int int32_t;
typedef unsigned char uint8_t;

struct S0
{
  uint8_t f3;
};

struct S2
{
  uint8_t f5;
};

struct S0 g_22 = {
  0
};

int32_t g_91;
const struct S2 *g_314;
const struct S2 **g_313 = &g_314;
struct S2 g_320[2] = {
  0
};

void
int32func (uint64p_34)
{
  for (g_22.f3 = 0; g_22.f3 <= 0; g_22.f3)
    {
    lbl_491:{
        if (1)
          {
            int32_t *l_453[2][7][7][1][1];
            int i, j, k, l, m;
            for (m; m; m++)
              l_453[i][j][k][l][m];
          }
        *g_313 = 0;
        if (g_91)
          goto lbl_491;
      }
    }
}
