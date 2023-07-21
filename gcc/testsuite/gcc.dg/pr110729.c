/* { dg-do compile { target { ! { nvptx*-*-* visium-*-* } } } } */
/* { dg-require-effective-target o_flag_in_section } */
/* { dg-options "-ffunction-sections -fpatchable-function-entry=2" } */
/* { dg-additional-options "-fno-pie" { target sparc*-*-* } } */

/* Verify there are three different link_to symbols for three
   .section __patchable_function_entries respectively.  */

int
f ()
{
  return 1;
}

int
g ()
{
  return 2;
}

int
h ()
{
  return 3;
}

/* { dg-final { scan-assembler-times {.section[\t ]*__patchable_function_entries,.*,\.LPFE0} 1 } }  */
/* { dg-final { scan-assembler-times {.section[\t ]*__patchable_function_entries,.*,\.LPFE1} 1 } }  */
/* { dg-final { scan-assembler-times {.section[\t ]*__patchable_function_entries,.*,\.LPFE2} 1 } }  */
