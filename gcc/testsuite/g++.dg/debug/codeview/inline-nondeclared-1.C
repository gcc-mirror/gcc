// { dg-do compile }
// { dg-options "-O3 -gcodeview -Wno-attributes" }
// { dg-final { scan-assembler "Lcv_inlineelines_start" } }

// codeview_abstract_function must record functions inlined by the optimizer,
// even if they were not declared inline.

__attribute__ ((always_inline)) static int
callee (int i)
{
  return i + 1;
}

int
caller (int i)
{
  return callee (i);
}
