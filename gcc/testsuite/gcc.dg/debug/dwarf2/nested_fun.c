/* As part of inlining, a BLOCK (described as DW_TAG_lexical_block DIE's) may
   be present both as an abstract instance and a concrete one in the DWARF
   output.  This testcase attempts to make sure that the concrete ones refer to
   the abstract ones thanks to the DW_AT_abstract_origin attribute.

   Such a back-link enables debuggers to make entities present in the abstract
   instance only available in concrete ones.  */

/* { dg-options "-O2 -g -std=gnu99 -gdwarf -dA" } */
/* { dg-final { scan-assembler-times "\\(DIE \\(0x.*\\) DW_TAG_lexical_block\\)\[^)\]*DW_AT_abstract_origin" 1 } } */

extern void *create (const char *);
extern void destroy (void *);
extern void do_nothing (char);

struct string
{
  const char *data;
  int lb;
  int ub;
};

int
main (void)
{
  void *o1 = create ("foo");

  void
  parent (void)
  {
    {
      void *o2 = create ("bar");

      int
      child (struct string s)
      {
	int i = s.lb;

	if (s.lb <= s.ub)
	  while (1)
	    {
	      char c = s.data[i - s.lb];
	      do_nothing (c);
	      if (c == 'o')
		return 1;
	      if (i == s.ub)
		break;
	      ++i;
	    }
	return 0;
      }

      int r;

      r = child ((struct string) {"baz", 1, 3});
      r = child ((struct string) {"qux", 2, 4});
      r = child ((struct string) {"foobar", 1, 6});
    }

    do_nothing (0);
  }

  parent ();
  return 0;
}
