/* { dg-additional-options "-O2" } */

int f(int l) {
  char *t_string = (char *)__builtin_calloc(l + 2, 1);
  char *end = t_string + l - 1;
  return '0' != *(end - 1); /* { dg-warning "leak of 't_string'" } */
}
