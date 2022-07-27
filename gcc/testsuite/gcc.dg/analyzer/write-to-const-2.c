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

const char buf[5] = { 0 }; /* { dg-message "declared here" } */

void test_read_only (void)
{
  read_only ((char *)buf);
}

void test_write_only (void)
{
  write_only ((char *)buf); /* { dg-warning "write to 'const' object 'buf'" } */
}

void test_read_write (void)
{
  read_write ((char *)buf); /* { dg-warning "write to 'const' object 'buf'" } */
}

void test_none (void)
{
  none ((char *)buf);
}

void test_read_only_with_size (void)
{
  read_only_with_size ((char *)buf, sizeof (buf));
}

void test_write_only_with_size (void)
{
  write_only_with_size ((char *)buf, sizeof (buf)); /* { dg-warning "write to 'const' object 'buf'" } */
}

void test_read_write_with_size (void)
{
  read_write_with_size ((char *)buf, sizeof (buf)); /* { dg-warning "write to 'const' object 'buf'" } */
}

void test_none_with_size (void)
{
  none_with_size ((char *)buf, sizeof (buf));
}
