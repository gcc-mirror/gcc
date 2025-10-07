/* { dg-do compile }  */
/* { dg-require-ifunc } */
/* { dg-options "-std=c23 -pedantic-errors" } */

// Value was 2, now is 1
int foo [[gnu::target_version("dotprod;priority=2")]] (void); /* { dg-message ".foo \\\[\\\[target_version\\\(.dotprod;priority=2.\\\)\\\]\\\]. was previously declared here with priority value of 2" } */
int foo [[gnu::target_version("dotprod;priority=1")]] (void) { return 2; } /* { dg-error ".foo \\\[\\\[target_version\\\(.dotprod;priority=1.\\\)\\\]\\\]. has an inconsistent function multi-version priority value" } */

// Value was 0, now is 2
int bar [[gnu::target_version("dotprod")]] (void); /* { dg-message ".bar \\\[\\\[target_version\\\(.dotprod.\\\)\\\]\\\]. was previously declared here with priority value of 0" } */
int bar [[gnu::target_version("dotprod;priority=2")]] (void) { return 2; } /* { dg-error ".bar \\\[\\\[target_version\\\(.dotprod;priority=2.\\\)\\\]\\\]. has an inconsistent function multi-version priority value" } */
