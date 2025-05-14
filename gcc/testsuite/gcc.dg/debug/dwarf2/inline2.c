/* Contributed by Dodji Seketeli <dodji@redhat.com>
   Origin: PR debug/37801

  Abstract instances (DW_TAG_subroutines having the DW_AT_inline attribute)
  of second and first were having a DW_TAG_lexical_block DIE wrongly
  representing the inlined calls to third (in second) and to
  second (in first). At the same time, main didn't have children
  DW_TAG_inlined_subroutine DIEs representing the inlined calls to
  first, second and third.

  The ideal goal here is to test that we have no superfluous
  DW_TAG_lexical_block DIE anymore, that abstract instances DIEs have
  no descendant DIE with a DW_AT_abstract_origin attribute, and that main has
  properly nested DW_TAG_inlined_subroutine DIEs for third, second and first.
*/

/* Explicitly use dwarf-2 because dwarf-5 might use DW_FORM_implicit_const
   which is hard to scan for. */
/* { dg-options "-O -g3 -gdwarf-2 -gno-strict-dwarf -dA -fgnu89-inline" } */
/* { dg-do compile } */

/* There are 6 inlined subroutines:
   - One for each subroutine inlined into main, that's 3.
   - One for earch subroutine inline into the out of line instances
     of third, second and first.  */
/* { dg-final { scan-assembler-times "\\(DIE \\(\[^\n\]*\\) DW_TAG_inlined_subroutine" 6 } } */

/* We should have no DW_TAG_lexical_block DIEs, all inline instances
   should have the first subblock elided to match the abstract instance
   layout.  */
/* { dg-final { scan-assembler-times "\\(DIE \\(\[^\n\]*\\) DW_TAG_lexical_block" 0 } } */

/* Each inline instance should have DW_AT_entry_pc and DW_AT_GNU_entry_view.  */
/* { dg-final { scan-assembler-times " DW_AT_entry_pc" 6 } } */
/* { dg-final { scan-assembler-times " DW_AT_GNU_entry_view" 6 } } */

/* There are 3 DW_AT_inline attributes: one per abstract inline instance.
   The value of the attribute must be 0x3, meaning the function was
   actually inlined.  */
/* { dg-final { scan-assembler-times  "(?:byte|data1)\[^\n\]*0x3\[^\n\]* DW_AT_inline" 3 } } */

volatile int *a;

inline void
third (int arg3)
{
  int var3 = arg3;
  a[0] = var3;
}

inline void
second (int arg2)
{
  int var2 = arg2;
  third (var2+1);
}

inline void
first (int arg1)
{
  int var1 = arg1;
  second (var1+1);
}

int
main ()
{
  int some_int = 1;
  first (some_int);
  return 0;
}


