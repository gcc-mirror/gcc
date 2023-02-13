extern int open(const char *, int mode);
#define O_RDONLY 0

struct {
  int fd_a;
  int fd_b;
} g;

int test (const char *path, int flag)
{
  int *target;
  target = flag ? &g.fd_a : &g.fd_b;
  *target = open (path, O_RDONLY);  
  return 0; /* { dg-bogus "leak of file descriptor" } */
}
