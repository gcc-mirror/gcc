/* Tests of C frontend's address space type-checking.  */
/* { dg-options "-std=gnu90 -fdiagnostics-show-caret" } */

/* Verify that we emit helpful diagnostics at a mismatching address space
   at a function call, and that the underlined locations are correct.  */

extern void expects_seg_gs (int i, void __seg_gs *param, int j); /* { dg-line "decl_line" } */

void
test_bad_call (void *ptr)
{
  expects_seg_gs (0, ptr, 1); /* { dg-line "err_line" } */
}

/* { dg-error "passing argument 2 of 'expects_seg_gs' from pointer to non-enclosed address space" "" { target *-*-* } err_line } */
/* { dg-begin-multiline-output "" }
   expects_seg_gs (0, ptr, 1);
                      ^~~
   { dg-end-multiline-output "" } */

/* { dg-message "expected '__seg_gs void \\*' but argument is of type 'void \\*'" "" { target *-*-* } decl_line } */
/* { dg-begin-multiline-output "" }
 extern void expects_seg_gs (int i, void __seg_gs *param, int j);
                                    ~~~~~~~~~~~~~~~^~~~~
   { dg-end-multiline-output "" } */
