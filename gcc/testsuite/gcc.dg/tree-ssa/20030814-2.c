/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom3" } */
    
extern void abort (void);

void
foo (int value)
{
  switch (value)
    {
    case 42:
	      if (value != 42)
		abort ();
    case 50:
      blah ();
    }
}

/* There should be no IF conditionals.  */
/* { dg-final { scan-tree-dump-times "if " 0 "dom3"} } */
 
/* { dg-final { cleanup-tree-dump "dom3" } } */
