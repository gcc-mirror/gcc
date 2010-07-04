int safe (int);

static unsigned foo (unsigned ui1, unsigned ui2)
{
  return ui1 + ui2;
}

int g_22;
int *volatile g_23 = &g_22;
int **g_282[8][10][1];
int *g_330 = &g_22;
volatile unsigned g_348;
int g_397;

void int32func (const unsigned char p_10)
{
  if (foo
      (~
       (p_10 |
	(*g_282[(unsigned long) g_397 % 8][(unsigned) g_22 % 10][g_348 % 1]) ==
	(*g_282[(unsigned long) g_397 % 8][(unsigned) g_22 % 10][g_348 % 1])),
       1))
    {
    }
  else if (*g_330 >=
	   safe (*g_23 ^
		 (**g_282[(unsigned long) g_397 % 8][(unsigned) g_22 % 10]
		  [g_348 % 1])) & **g_282[8][10][1], 1)
    {
    }
}


