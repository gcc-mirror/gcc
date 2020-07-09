/* We ensure that -Wpedantic is off since it complains about the trampolines
   we explicitly want to test.  */
/* { dg-additional-options "-mharden-sls=retbr -Wno-pedantic " } */
/*
   Ensure that the SLS hardening of RET and BR leaves no unprotected RET/BR
   instructions.
  */
typedef int (foo) (int, int);
typedef void (bar) (int, int);
struct sls_testclass {
    foo *x;
    bar *y;
    int left;
    int right;
};

int
retbr_sibcall_value_insn (struct sls_testclass x)
{
  return x.x(x.left, x.right);
}

void
retbr_sibcall_insn (struct sls_testclass x)
{
  x.y(x.left, x.right);
}

/* Aim to test two different returns.
   One that introduces a tail call in the middle of the function, and one that
   has a normal return.  */
int
retbr_multiple_returns (struct sls_testclass x)
{
  int temp;
  if (x.left % 10)
    return x.x(x.left, 100);
  else if (x.right % 20)
    {
      return x.x(x.left * x.right, 100);
    }
  temp = x.left % x.right;
  temp *= 100;
  temp /= 2;
  return temp % 3;
}

void
retbr_multiple_returns_void (struct sls_testclass x)
{
  if (x.left % 10)
    {
      x.y(x.left, 100);
    }
  else if (x.right % 20)
    {
      x.y(x.left * x.right, 100);
    }
  return;
}

/* Testing the casesi jump via register.  */
__attribute__ ((optimize ("Os")))
int
retbr_casesi_dispatch (struct sls_testclass x)
{
  switch (x.left)
    {
    case -5:
      return -2;
    case -3:
      return -1;
    case 0:
      return 0;
    case 3:
      return 1;
    case 5:
      break;
    default:
      __builtin_unreachable ();
    }
  return x.right;
}

/* Testing the BR in trampolines is mitigated against.  */
void f1 (void *);
void f3 (void *, void (*)(void *));
void f2 (void *);

int
retbr_trampolines (void *a, int b)
{
  if (!b)
    {
      f1 (a);
      return 1;
    }
  if (b)
    {
      void retbr_tramp_internal (void *c)
      {
	if (c == a)
	  f2 (c);
      }
      f3 (a, retbr_tramp_internal);
    }
  return 0;
}

/* Testing the indirect_jump pattern.  */
void
retbr_indirect_jump (int *buf)
{
  __builtin_longjmp(buf, 1);
}

/* Ensure there are no BR or RET instructions which are not directly followed
   by a speculation barrier.  */
/* { dg-final { scan-assembler-not {\t(br|ret|retaa|retab)\tx[0-9][0-9]?\n\t(?!dsb\tsy\n\tisb|sb)} } } */
