/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */
                                                                                
static int copying_arguments;

int
foo ()
{
  unsigned int regno;
  if (regno < 53 && copying_arguments)
    if (regno >= 53)
	return 1;
}

/* There should be no IF conditionals.  */
/* { dg-final { scan-tree-dump-times "if " 0 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
