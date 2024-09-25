/* { dg-do link } */
/* { dg-do run { target nvptx_runtime_alias_ptx } } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options nvptx_alias_ptx } */

#include "alias-3.c"

/* Static and inlined, so it's deleted.  */
/* { dg-final { scan-assembler-times {(?n)^// BEGIN FUNCTION DECL: __f$} 0 } }
   { dg-final { scan-assembler-times {(?n)^\.visible \.func __f;$} 0 } }
   { dg-final { scan-assembler-times {(?n)^// BEGIN FUNCTION DEF: __f$} 0 } }
   { dg-final { scan-assembler-times {(?n)^\.visible \.func __f$} 0 } } */

/* Inlined, so no alias.  */

/* { dg-final { scan-assembler-times {(?n)^// BEGIN FUNCTION DECL: f$} 0 } }
   { dg-final { scan-assembler-times {(?n)^\.visible \.func f;$} 0 } }
   { dg-final { scan-assembler-times {(?n)^// BEGIN FUNCTION DEF: f$} 0 } }
   { dg-final { scan-assembler-times {(?n)^\.alias f,__f;$} 0 } } */

/* { dg-final { scan-assembler-times {(?n)\tcall __f;$} 0 } }
   { dg-final { scan-assembler-times {(?n)\tcall f;$} 0 } } */
