int *
__errno_location (void);

long int
read (int, void *, unsigned long int);

struct IOBUF {
  int fd;
};

void
do_getline_end_data (struct IOBUF *iop, int tree)
{
  char end_data;

  if (tree)
    *__errno_location () = 0;

  read (iop->fd, &end_data, sizeof end_data);
}
