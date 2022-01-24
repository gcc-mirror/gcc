/* PR sanitizer/104158 */
/* { dg-do compile } */
/* { dg-options "-fsanitize-coverage=trace-cmp,trace-cmp -fdump-tree-optimized" } */
/* { dg-error "invalid argument in option '-fsanitize-coverage=trace-cmp,trace-cmp'" "" { target *-*-* } 0 } */
/* { dg-message "'trace-cmp' specified multiple times in the same option" "" { target *-*-* } 0 } */
