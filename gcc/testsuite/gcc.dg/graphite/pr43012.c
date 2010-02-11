/* This testcase is from PR43012.
   You will need CLooG-PPL 0.15.8 or later to have this testcase fixed.  */

/* { dg-do run } */
/* { dg-options "-O2 -floop-strip-mine" } */

extern void abort (void);

#ifdef DBG
extern int printf (const char *, ...);
#endif

#define LAST_TOKEN 534
#define FLOAT_FUNCT_TOKEN 64
#define VECTOR_FUNCT_TOKEN 77
#define COLOUR_KEY_TOKEN 89

int Table[LAST_TOKEN];

void
pre_init_tokenizer ()
{
  int i;

  for (i = 0; i < LAST_TOKEN; i++)
    {
      Table[i] = i;
      if (i < FLOAT_FUNCT_TOKEN)
	Table[i] = FLOAT_FUNCT_TOKEN;
      else
	{
	  if (i < VECTOR_FUNCT_TOKEN)
	    Table[i] = VECTOR_FUNCT_TOKEN;
	  else
	    {
	      if (i < COLOUR_KEY_TOKEN)
		Table[i] = COLOUR_KEY_TOKEN;
	    }
	}
    }
}

void
check ()
{
  int i;

  for (i = 0; i < FLOAT_FUNCT_TOKEN; i++)
    if (Table[i] != FLOAT_FUNCT_TOKEN)
      abort ();
  for (i = FLOAT_FUNCT_TOKEN; i < VECTOR_FUNCT_TOKEN; i++)
    if (Table[i] != VECTOR_FUNCT_TOKEN)
      abort ();
  for (i = VECTOR_FUNCT_TOKEN; i < COLOUR_KEY_TOKEN; i++)
    if (Table[i] != COLOUR_KEY_TOKEN)
      abort ();
  for (i = COLOUR_KEY_TOKEN; i < LAST_TOKEN; i++)
    if (Table[i] != i)
      abort ();
}

int
main ()
{
  int i;

  pre_init_tokenizer ();
#ifdef DBG
  for (i = 0; i < LAST_TOKEN; i++)
    printf ("%3d: %d\n", i, Table[i]);
#endif
  check ();
  return 0;
}
