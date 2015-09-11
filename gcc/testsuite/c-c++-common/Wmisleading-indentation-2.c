/* { dg-options "-Wmisleading-indentation" } */
/* { dg-do compile } */

/* Based on get_attr_athlon_decode from the generated insn-attrtab.c
   for x86_64.
   A #line directive, followed by a very long line to ensure that
   we're in a fresh line_map.

   This should not generate a misleading indentation warning.

   This needs to be in its own file since -Wmisleading-indentation stops
   after seeing a #line directive.  */
void fn ()
{
  switch (0)
    {
#line 6 "../../../../src/gcc/testsuite/c-c++-common/Wmisleading-indentation-2.md"
    case 0:
      if (0)
        {
	  return;
        }

    case 1:
      if (0)
        {
	  return;
        }
      else
        {
	  return;
        }

      /**********************************************************************************************************************************/
      if (0)
        {
	  return;
        }
      else if (0)
        {
	  return;
        }
      else if (0)
        {
	  return;
        }
      else
        {
	  return;
        }

    default:
      return;

    }
}
