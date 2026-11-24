/* PR analyzer/106283 */
/* Test that an incompatible argument type is not treated as closefrom.  */

extern int close (int fd);
extern int closefrom (unsigned long lowfd);
extern int open (const char *, int mode);
#define O_RDONLY 0

void
test_incompatible_arg_still_leaks (const char *path)
{
  int fd = open (path, O_RDONLY);
  if (fd < 0)
    return;
  closefrom (0);
} /* { dg-warning "leak of file descriptor 'fd'" } */

void
test_incompatible_arg_no_double_close (const char *path)
{
  int fd = open (path, O_RDONLY);
  if (fd < 0)
    return;
  closefrom (0);
  close (fd); /* { dg-bogus "double 'close'" } */
}
