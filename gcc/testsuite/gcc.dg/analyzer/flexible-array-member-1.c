#include <stdlib.h>
#include <string.h>

struct str {
  size_t len;
  char data[];
};

struct str *
test_const_size (void)
{
  struct str *str = malloc(sizeof(str) + 10);
  if (str) {
    str->len = 10;
    memset(str->data, 'x', 10);
    return str;
  }
  return NULL;
}

struct str *
test_const_size_oob_1 (void)
{
  /* Forgetting to add space for the trailing array.  */
  struct str *str = malloc(sizeof(str));
  if (str) {
    str->len = 10;
    memset(str->data, 'x', 10); /* { dg-warning "heap-based buffer overflow" "Wanalyzer-out-of-bounds" } */
    /* { dg-warning "'memset' writing 10 bytes into a region of size 0 overflows the destination" "Wstringop-overflow" { target *-*-* } .-1 } */
    return str;
  }
  return NULL;
}

struct str *
test_const_size_oob_2 (void)
{
  struct str *str = malloc(sizeof(str) + 10);
  if (str) {
    str->len = 10;
    /* Using the wrong size here.  */
    memset(str->data, 'x', 11); /* { dg-warning "heap-based buffer overflow" "Wanalyzer-out-of-bounds" } */
    /* { dg-warning "'memset' writing 11 bytes into a region of size 10 overflows the destination" "Wstringop-overflow" { target *-*-* } .-1 } */
    return str;
  }
  return NULL;
}

struct str *
test_symbolic_size (size_t len)
{
  struct str *str = malloc(sizeof(str) + len);
  if (str) {
    str->len = len;
    memset(str->data, 'x', len);
    return str;
  }
  return NULL;
}

struct str *
test_symbolic_size_oob (size_t len)
{
  /* Forgetting to add space for the trailing array.  */
  struct str *str = malloc(sizeof(str));
  if (str) {
    str->len = len;
    memset(str->data, 'x', len); /* { dg-warning "heap-based buffer overflow" "PR analyzer/98247" { xfail *-*-* } } */
    // TODO(xfail): we don't yet complain about this case, which occurs when len > 0
    return str;
  }
  return NULL;
}

struct str *
test_symbolic_size_with_terminator (size_t len)
{
  struct str *str = malloc(sizeof(str) + len + 1);
  if (str) {
    str->len = len;
    memset(str->data, 'x', len);
    str->data[len] = '\0';
    return str;
  }
  return NULL;
}

struct str *
test_symbolic_size_with_terminator_oob (size_t len)
{
  /* Forgetting to add 1 for the terminator.  */
  struct str *str = malloc(sizeof(str) + len);
  if (str) {
    str->len = len;
    memset(str->data, 'x', len);
    str->data[len] = '\0'; /* { dg-warning "heap-based buffer overflow" } */
    return str;
  }
  return NULL;
}
