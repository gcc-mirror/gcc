/* Verify that we don't offer non-recoverable options as suggestions
   for misspelled -fsanitize-recover= arguments (PR driver/78877).  */

/* { dg-do compile } */
/* { dg-options "-fsanitize-recover=threed" } */
/* Ensure we don't offer non-recoverable "-fsanitize-recover=thread"
   as a suggestion.  */
/* { dg-bogus "did you mean" "" { target *-*-* } 0 } */
/* { dg-error "unrecognized argument to '-fsanitize-recover=' option: 'threed'" "" { target *-*-* } 0 } */
