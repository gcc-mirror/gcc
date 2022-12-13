int open(const char *, int mode);
void close(int fd);
int write (int fd, void *buf, int nbytes);
int read (int fd, void *buf, int nbytes);

#define O_RDONLY 0
#define O_WRONLY 1
#define O_RDWR 2
#define O_ACCMODE 0x3

void f (int fd) __attribute__((fd_arg(1))); /* { dg-message "argument 1 of 'f' must be an open file descriptor, due to '__attribute__\\(\\(fd_arg\\(1\\)\\)\\)'" } */

void
test_1 (const char *path)
{
    int fd = open (path, O_RDWR);
    close(fd);
    f(fd); /* { dg-warning "'f' on closed file descriptor 'fd'" } */
      /* { dg-message "\\(3\\) 'f' on closed file descriptor 'fd'; 'close' was at \\(2\\)" "" { target *-*-* } .-1 } */
}

void g (int fd) __attribute__((fd_arg_read(1))); /* { dg-message "argument 1 of 'g' must be a readable file descriptor, due to '__attribute__\\(\\(fd_arg_read\\(1\\)\\)\\)'" } */

void
test_2 (const char *path)
{
  int fd = open (path, O_WRONLY);
  if (fd != -1)
  {
    g (fd); /* { dg-warning "'g' on write-only file descriptor 'fd'" } */
  }
  close (fd);
}

void h (int fd) __attribute__((fd_arg_write(1))); /* { dg-message "argument 1 of 'h' must be a writable file descriptor, due to '__attribute__\\(\\(fd_arg_write\\(1\\)\\)\\)'" } */
void
test_3 (const char *path)
{
  int fd = open (path, O_RDONLY);
  if (fd != -1)
  {
    h (fd); /* { dg-warning "'h' on read-only file descriptor 'fd'" } */
  }
  close(fd);
}

void ff (int fd) __attribute__((fd_arg(1))); /* { dg-message "argument 1 of 'ff' must be an open file descriptor, due to '__attribute__\\(\\(fd_arg\\(1\\)\\)\\)'" } */

void test_4 (const char *path)
{
  int fd = open (path, O_RDWR);
  ff (fd); /* { dg-warning "'ff' on possibly invalid file descriptor 'fd'" } */
  close(fd);
}