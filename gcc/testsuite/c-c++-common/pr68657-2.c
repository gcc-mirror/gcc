/* PR c/68657 */
/* { dg-do compile } */
/* { dg-options "-Werror=larger-than=65536" } */
/* { dg-require-effective-target ptr32plus } */

int a[131072];	/* { dg-error "size of .a. 524288 bytes exceeds maximum object size 65536" } */
int b[1024];	/* { dg-bogus "size" } */

/* { dg-prune-output "treated as errors" } */
