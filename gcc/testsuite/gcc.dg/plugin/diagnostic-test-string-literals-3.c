/* Similar to diagnostic-test-string-literals-1.c, but with
   -ftrack-macro-expansion=0.  */

/* { dg-do compile } */
/* { dg-options "-O -ftrack-macro-expansion=0" } */

extern void __emit_string_literal_range (const void *literal, int caret_idx,
					 int start_idx, int end_idx);

void
test_simple_string_literal (void)
{
  __emit_string_literal_range ("0123456789", /* { dg-error "unable to read substring location: track_macro_expansion != 2" } */
			       6, 6, 7);
}

void
test_concatenated_string_literal (void)
{
  __emit_string_literal_range ("01234" "56789", /* { dg-error "unable to read substring location: track_macro_expansion != 2" } */
			       4, 3, 6);
}

/* To reproduce PR preprocessor/78324, the macro name should start
   with the letter 'R'.  */

void
test_macro (void)
{
#define RANGE "01234"
  __emit_string_literal_range (RANGE /* { dg-error "unable to read substring location: track_macro_expansion != 2" } */
                               "56789",
                               4, 3, 6);
#undef RANGE
}

void
test_multitoken_macro (void)
{
#define RANGE ("0123456789")
  __emit_string_literal_range (RANGE, 4, 3, 6); /* { dg-error "unable to read substring location: track_macro_expansion != 2" } */
#undef RANGE
}
