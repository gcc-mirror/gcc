/* { dg-do preprocess } */
/* { dg-options "-std=gnu99 -I /jolly/well/better/not/exist -Wmissing-include-dirs -fno-show-column" } */

/* Test that -Wmissing-include-dirs issues a warning when a specified
   directory does not exist.  Source Ben Elliston, 2004-05-13.  */

/* { dg-warning "No such file or directory" "-Wmissing-include-dirs" { target *-*-* } 0 } */
