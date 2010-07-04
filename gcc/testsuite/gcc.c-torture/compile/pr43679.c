unsigned g_5;
int g_7;
int g_23[2];
int *g_29 = &g_23[0];
int **g_59;
unsigned long g_186;

int foo (int, int);
int bar (int);

void func_37 (long p_38)
{
  int *l_39 = &g_7;
  *l_39 = (*l_39
             ||
             (foo
              (((*g_29 != *l_39, ((bar (g_59 != &l_39), 0), 0))),
               0)));
  foo (*l_39, 0);
  int **l_256 = &l_39;
  {
    for (0; g_186; 0)
      {
        *l_256 = *l_256;
        if (g_5)
          goto lbl_270;
        *l_39 &= 0;
      }
  }
lbl_270:
  ;
}

