/* PR middle-end/31448, this used to ICE during expand because
   reduce_to_bit_field_precision was not ready to handle constants. */

typedef struct _st {
    int iIndex : 24;
    int iIndex1 : 24;
} st;
st *next;
void g(void)
{
    st *next = 0;
    int nIndx;
    const static int constreg[] = { 0,};
    nIndx = 0;
    next->iIndex = constreg[nIndx];
}
void f(void)
{
    int nIndx;
    const static int constreg[] = { 0xFEFEFEFE,};
    nIndx = 0;
    next->iIndex = constreg[nIndx];
    next->iIndex1 = constreg[nIndx];
}
int main(void)
{
  st a;
  next = &a;
  f();
  if (next->iIndex != 0xFFFEFEFE)
    __builtin_abort ();
  if (next->iIndex1 != 0xFFFEFEFE)
    __builtin_abort ();
  return 0;
}

