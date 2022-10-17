/* PR middle-end/92333 - missing variable name referencing VLA in warnings
   PR middle-end/82608 - missing -Warray-bounds on an out-of-bounds VLA index
   { dg-do compile }
   { dg-options "-O2 -Wall -fno-tree-vectorize" }  */

void sink (void*);

void test_struct_char_vla_location (void)
{
  unsigned nelts = 7;

  struct {
    char cvla[nelts]; // { dg-message "declared here|while referencing" }
  } s;

  s.cvla[0] = __LINE__;
  s.cvla[nelts - 1] = 0;
  s.cvla[nelts] = 0;  // { dg-warning "\\\[-Warray-bounds" }

  sink (&s);
}
