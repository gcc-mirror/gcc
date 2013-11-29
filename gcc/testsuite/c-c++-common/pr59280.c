/* PR c/59280 */
/* { dg-do compile } */

void bar (char *) __attribute__((constructor(foo))); /* { dg-error "constructor priorities must be integers|was not declared|constructor priorities are not supported" } */
