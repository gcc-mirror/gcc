typedef __SIZE_TYPE__ size_t;

void read_only (void *)
  __attribute__ ((access (read_only, 1)));
void write_only (void *) /* { dg-message "parameter 1 of 'write_only' marked with attribute 'access \\(write_only, 1\\)'" } */
  __attribute__ ((access (write_only, 1)));
void read_write (void *) /* { dg-message "parameter 1 of 'read_write' marked with attribute 'access \\(read_write, 1\\)'" } */
  __attribute__ ((access (read_write, 1)));
void none (void *)
  __attribute__ ((access (none, 1)));
void read_only_with_size (void *, size_t)
  __attribute__ ((access (read_only, 1, 2)));
void write_only_with_size (void *, size_t) /* { dg-message "parameter 1 of 'write_only_with_size' marked with attribute 'access \\(write_only, 1, 2\\)'" } */
  __attribute__ ((access (write_only, 1, 2)));
void read_write_with_size (void *, size_t) /* { dg-message "parameter 1 of 'read_write_with_size' marked with attribute 'access \\(read_write, 1, 2\\)'" } */
  __attribute__ ((access (read_write, 1, 2)));
void none_with_size (void *, size_t)
  __attribute__ ((access (none, 1, 2)));

void test_read_only (void)
{
  const char *str = "hello world";
  read_only ((char *)str);
}

void test_write_only (void)
{
  const char *str = "hello world";
  write_only ((char *)str); /* { dg-warning "write to string literal" } */
}

void test_read_write (void)
{
  const char *str = "hello world";
  read_write ((char *)str); /* { dg-warning "write to string literal" } */
}

void test_none (void)
{
  const char *str = "hello world";
  none ((char *)str);
}

void test_read_only_with_size (void)
{
  const char *str = "hello world";
  read_only_with_size ((char *)str, sizeof (str));
}

void test_write_only_with_size (void)
{
  const char *str = "hello world";
  write_only_with_size ((char *)str, sizeof (str)); /* { dg-warning "write to string literal" } */
}

void test_read_write_with_size (void)
{
  const char *str = "hello world";
  read_write_with_size ((char *)str, sizeof (str)); /* { dg-warning "write to string literal" } */
}

void test_none_with_size (void)
{
  const char *str = "hello world";
  none_with_size ((char *)str, sizeof (str));
}
