typedef long int ssize_t;
typedef long unsigned int size_t;

extern int open(const char* __file, int __oflag, ...) __attribute__((__nonnull__(1)));
extern int close(int __fd);
extern ssize_t read(int __fd, void* __buf, size_t __nbytes);

struct ring
{
  char buf[1024];
};

int
test(const char* name)
{
  struct ring ring;
  int fd;
  int ret;

  fd = open(name, 00);
  if (fd < 0)
    return 0;

  ret = read(fd, &ring, sizeof(ring));
  close(fd);

  if (ret != sizeof(ring))
    return 1;

  if (ring.buf[0] > 1) /* { dg-bogus "use of uninitialized value" } */
    return 2;
  return 3;
}
