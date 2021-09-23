/* { dg-do compile } */
/* { dg-options "-fsanitize-coverage=tracecmp" } */

/* { dg-error "unrecognized argument in option '-fsanitize-coverage=tracecmp'" "" { target *-*-* } 0 } */
/* { dg-message "valid arguments to '-fsanitize-coverage=' are: trace-cmp trace-pc; did you mean 'trace-cmp'?" "" { target *-*-* } 0 } */
