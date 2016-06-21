/* PR c/68657 */
/* { dg-do compile } */
/* { dg-options "-Werror=larger-than=65536" } */
/* { dg-require-effective-target ptr32plus } */

int a[131072];	/* { dg-error "size of 'a' is \[1-9]\[0-9]* bytes" } */
int b[1024];	/* { dg-bogus "size of 'b' is \[1-9]\[0-9]* bytes" } */

/* { dg-prune-output "treated as errors" } */
