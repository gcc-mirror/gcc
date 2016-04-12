/* Executable testcase for 'output flags.'  */
/* { dg-do run } */

char TestC ()
{
  char r;

  __asm__ ("stc" : "=@ccc"(r));
  if (r)
  {
    __asm__ ("clc" : "=@ccnc"(r));
    if (r)
      return 1;
  }
  return 0;
}

char TestE ()
{
  char r;

  /* 1 equals 1.  */
  __asm__ ("cmp $1, %1" : "=@cce"(r) : "r" (1));
  if (r)
  {
    /* 1 not equals 2.  */
    __asm__ ("cmp $2, %1" : "=@ccne"(r) : "r" (1));
    if (r)
      return 1;
  }
  return 0;
}

char TestZ ()
{
  char r;

  /* 1 equals 1.  */
  __asm__ ("cmp $1, %1" : "=@ccz"(r) : "r" (1));
  if (r)
  {
    /* 1 not equals 2.  */
    __asm__ ("cmp $2, %1" : "=@ccnz"(r) : "r" (1));
    if (r)
      return 1;
  }
  return 0;
}

char TestA ()
{
  char r;

  /* 1 a 0.  */
  __asm__ ("cmp $0, %1" : "=@cca"(r) : "r" (1));
  if (r)
  {
    /* 1 na 2.  */
    __asm__ ("cmp $2, %1" : "=@ccna"(r) : "r" (1));
    if (r)
    {
      /* 1 na 1.  */
      __asm__ ("cmp $1, %1" : "=@ccna"(r) : "r" (1));
      if (r)
	return 1;
    }
  }
  return 0;
}

char TestAE ()
{
  char r;

  /* 1 ae 0.  */
  __asm__ ("cmp $0, %1" : "=@ccae"(r) : "r" (1));
  if (r)
  {
    /* 1 nae 2.  */
    __asm__ ("cmp $2, %1" : "=@ccnae"(r) : "r" (1));
    if (r)
    {
      /* 1 ae 1.  */
      __asm__ ("cmp $1, %1" : "=@ccae"(r) : "r" (1));
      if (r)
	return 1;
    }
  }
  return 0;
}

char TestB ()
{
  char r;

  /* 1 b 2.  */
  __asm__ ("cmp $2, %1" : "=@ccb"(r) : "r" (1));
  if (r)
  {
    /* 1 nb 0.  */
    __asm__ ("cmp $0, %1" : "=@ccnb"(r) : "r" (1));
    if (r)
    {
      /* 1 nb 1.  */
      __asm__ ("cmp $1, %1" : "=@ccnb"(r) : "r" (1));
      if (r)
	return 1;
    }
  }
  return 0;
}

char TestBE ()
{
  char r;

  /* 1 be 2.  */
  __asm__ ("cmp $2, %1" : "=@ccbe"(r) : "r" (1));
  if (r)
  {
    /* 1 nbe 0.  */
    __asm__ ("cmp $0, %1" : "=@ccnbe"(r) : "r" (1));
    if (r)
    {
      /* 1 be 1.  */
      __asm__ ("cmp $1, %1" : "=@ccbe"(r) : "r" (1));
      if (r)
	return 1;
    }
  }
  return 0;
}

char TestG ()
{
  char r;

  /* 1 g 0.  */
  __asm__ ("cmp $0, %1" : "=@ccg"(r) : "r" (1));
  if (r)
  {
    /* 1 ng 2.  */
    __asm__ ("cmp $2, %1" : "=@ccng"(r) : "r" (1));
    if (r)
    {
      /* 1 ng 1.  */
      __asm__ ("cmp $1, %1" : "=@ccng"(r) : "r" (1));
      if (r)
	return 1;
    }
  }
  return 0;
}

char TestGE ()
{
  char r;

  /* 1 ge 0.  */
  __asm__ ("cmp $0, %1" : "=@ccge"(r) : "r" (1));
  if (r)
  {
    /* 1 nge 2.  */
    __asm__ ("cmp $2, %1" : "=@ccnge"(r) : "r" (1));
    if (r)
    {
      /* 1 ge 1.  */
      __asm__ ("cmp $1, %1" : "=@ccge"(r) : "r" (1));
      if (r)
	return 1;
    }
  }
  return 0;
}

char TestL ()
{
  char r;

  /* 1 l 2.  */
  __asm__ ("cmp $2, %1" : "=@ccl"(r) : "r" (1));
  if (r)
  {
    /* 1 nl 0.  */
    __asm__ ("cmp $0, %1" : "=@ccnl"(r) : "r" (1));
    if (r)
    {
      /* 1 nl 1.  */
      __asm__ ("cmp $1, %1" : "=@ccnl"(r) : "r" (1));
      if (r)
	return 1;
    }
  }
  return 0;
}

char TestLE ()
{
  char r;

  /* 1 le 2.  */
  __asm__ ("cmp $2, %1" : "=@ccle"(r) : "r" (1));
  if (r)
  {
    /* 1 nle 0.  */
    __asm__ ("cmp $0, %1" : "=@ccnle"(r) : "r" (1));
    if (r)
    {
      /* 1 le 1.  */
      __asm__ ("cmp $1, %1" : "=@ccle"(r) : "r" (1));
      if (r)
	return 1;
    }
  }
  return 0;
}

char TestO ()
{
  char r;
  unsigned char res = 128;

  /* overflow.  */
  __asm__ ("addb $128, %1" : "=@cco"(r), "+r"(res));
  if (r)
  {
    /* not overflow.  */
    __asm__ ("addb $1, %1" : "=@ccno"(r), "+r"(res));
    if (r)
      return 1;
  }
  return 0;
}

char TestP ()
{
  char r, res = 1;

  /* even # bits.  */
  __asm__ ("addb $2, %1" : "=@ccp"(r), "+r"(res));
  if (r)
  {
    /* odd # bits.  */
    __asm__ ("addb $1, %1" : "=@ccnp"(r), "+r"(res));
    if (r)
      return 1;
  }
  return 0;
}

char TestS ()
{
  char r, res = 1;

  /* sign bit set.  */
  __asm__ ("addb $128, %1" : "=@ccs"(r), "+r"(res));
  if (r)
  {
    /* sign bit not set.  */
    __asm__ ("subb $128, %1" : "=@ccns"(r), "+r"(res));
    if (r)
      return 1;
  }
  return 0;
}

/* dg-do treats exit code of 0 as success.  */
int main ()
{
  if (TestC () && TestE () && TestZ () && TestA ()
      && TestAE () && TestB () && TestBE () && TestG ()
      && TestGE () && TestL () && TestLE () && TestO ()
      && TestP ()  && TestS ())
    return 0;
  __builtin_abort ();
}
