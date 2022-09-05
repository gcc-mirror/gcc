/* { dg-do compile } */
/* { dg-options "-mexplicit-relocs" } */

__thread int x __attribute__((model("extreme"))); /* { dg-error "attribute cannot be specified for thread-local variables" } */
register int y __asm__("tp") __attribute__((model("extreme"))); /* { dg-error "attribute cannot be specified for register variables" } */
int z __attribute__((model(114))); /* { dg-error "invalid argument" } */
int t __attribute__((model("good"))); /* { dg-error "invalid argument" } */
