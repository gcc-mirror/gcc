/* { dg-do run } */

#define length 16
typedef int type;
typedef type v8i64 __attribute__((vector_size(sizeof(type)*length)));
v8i64 g6, g27;
_Bool main_c14;

__attribute__((noipa))
static
void f(v8i64 *a)
{
  for(int i =0;i<length;i++)
    if ((*a)[i] != -1)
      __builtin_abort();
}

int main()
{
  if (main_c14) goto lbl_br43;
lbl_bf7:
  g6 = 0 <= g27;
  g27 = g27 <= g27;
  g27 = g6 > g27;
lbl_br43:
  g6 = g6 | g27;
  if (main_c14) goto lbl_bf7;
  g6 = g6 * g27 == 0;
  f(&g6);
}
