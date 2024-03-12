/* Tests for precision-of-wording within malloc warnings.  */

typedef __SIZE_TYPE__ size_t;
extern void *malloc(size_t);
extern void free(void *);
extern char *strcpy(char *__restrict __dest, const char *__restrict __src)
    __attribute__((__nothrow__, __leaf__)) __attribute__((__nonnull__(1, 2)));

void test_1 (void)
{
  void *p = malloc (1024); /* { dg-message "\\(1\\) this call could return NULL" } */
  strcpy ((char *)p, "hello world"); /* { dg-warning "use of possibly-NULL 'p' where non-null expected" "warning" } */
  /* { dg-message "\\(2\\) argument 1 \\('p'\\) from \\(1\\) could be NULL where non-null expected" "event" { target *-*-* } .-1 } */
  free (p);
}

int *test_2 (void)
{
  int *i = (int *) malloc (sizeof (int)); /* { dg-message "\\(1\\) this call could return NULL" } */
  *i = 42; /* { dg-warning "dereference of possibly-NULL 'i'" "warning" } */
  /* { dg-message "\\(2\\) 'i' could be NULL: unchecked value from \\(1\\)" "event" { target *-*-* } .-1 } */
  return i; 
}
