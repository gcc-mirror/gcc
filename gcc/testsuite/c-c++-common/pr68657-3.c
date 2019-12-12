/* PR c/68657 */
/* { dg-do compile } */
/* { dg-require-effective-target ptr32plus } */

#pragma GCC diagnostic error "-Wlarger-than=65536"
int a[131072];	/* { dg-error "size of 'a' \[1-9\]\[0-9\]* bytes exceeds maximum object size 65536" } */
int b[1024];	/* { dg-bogus "size" } */
#pragma GCC diagnostic ignored "-Wlarger-than=65536"
int c[131072];	/* { dg-bogus "size" } */
int d[1024];	/* { dg-bogus "size" } */
#pragma GCC diagnostic warning "-Wlarger-than=65536"
int e[131072];	/* { dg-warning "size of 'e' \[1-9\]\[0-9\]* bytes exceeds maximum object size 65536" } */
int f[1024];	/* { dg-bogus "size" } */
/* { dg-prune-output "treated as errors" } */
