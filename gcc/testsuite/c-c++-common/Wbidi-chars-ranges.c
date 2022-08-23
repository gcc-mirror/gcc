/* PR preprocessor/103026 */
/* { dg-do compile } */
/* { dg-options "-Wbidi-chars=unpaired,ucn -fdiagnostics-show-caret" } */
/* Verify that we escape and underline pertinent bidirectional
   control characters when quoting the source.  */

int test_unpaired_bidi () {
    int isAdmin = 0;
    /*‮ } ⁦if (isAdmin)⁩ ⁦ begin admins only */
/* { dg-warning "bidirectional" "" { target *-*-* } .-1 } */
#if 0
   { dg-begin-multiline-output "" }
     /*<U+202E> } <U+2066>if (isAdmin)<U+2069> <U+2066> begin admins only */
       ~~~~~~~~                                ~~~~~~~~                    ^
       |                                       |                           |
       |                                       |                           end of bidirectional context
       U+202E (RIGHT-TO-LEFT OVERRIDE)         U+2066 (LEFT-TO-RIGHT ISOLATE)
   { dg-end-multiline-output "" }
#endif

        __builtin_printf("You are an admin.\n");
    /* end admins only ‮ { ⁦*/
/* { dg-warning "bidirectional" "" { target *-*-* } .-1 } */
#if 0
   { dg-begin-multiline-output "" }
     /* end admins only <U+202E> { <U+2066>*/
                        ~~~~~~~~   ~~~~~~~~ ^
                        |          |        |
                        |          |        end of bidirectional context
                        |          U+2066 (LEFT-TO-RIGHT ISOLATE)
                        U+202E (RIGHT-TO-LEFT OVERRIDE)
   { dg-end-multiline-output "" }
#endif

    return 0;
}

int LRE_‪_PDF_\u202c;
/* { dg-warning "mismatch" "" { target *-*-* } .-1 } */
#if 0
   { dg-begin-multiline-output "" }
 int LRE_<U+202A>_PDF_\u202c;
         ~~~~~~~~     ^~~~~~
   { dg-end-multiline-output "" }
#endif

const char *s1 = "LRE_‪_PDF_\u202c";
/* { dg-warning "mismatch" "" { target *-*-* } .-1 } */
#if 0
   { dg-begin-multiline-output "" }
 const char *s1 = "LRE_<U+202A>_PDF_\u202c";
                       ~~~~~~~~     ^~~~~~
   { dg-end-multiline-output "" }
#endif
