/* PR analyzer/106283 */
/* Test that an incompatible return type is not treated as closefrom.  */

extern int close (int fd);
extern void *closefrom (int lowfd);
extern int open (const char *, int mode);
#define O_RDONLY 0

void
test_incompatible_return_no_double_close (const char *path)
{
  int fd = open (path, O_RDONLY);
  if (fd < 0)
    return;
  closefrom (0);
  close (fd); /* { dg-bogus "double 'close'" } */
}

void
test_incompatible_return_still_leaks (const char *path)
{
  int fd = open (path, O_RDONLY);
  if (fd < 0)
    return;
  closefrom (0);
} /* { dg-warning "leak of file descriptor 'fd'" } */
