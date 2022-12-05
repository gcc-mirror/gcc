/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

struct hostent {
    char **h_addr_list;
};
struct hostent *gethostbyname(const char*);
int socket(void);
int close(int);
int connect(int, const char*);

int get_tcp_socket(const char *machine)
{
  struct hostent *hp;
  int s42, x;
  char **addr;

  hp = gethostbyname(machine);
  x = 0;
  for (addr = hp->h_addr_list; *addr; addr++)
    {
      s42 = socket();
      if (s42 < 0)
	return -1;
      x = connect(s42, *addr);
      if (x == 0)
	break;
      close(s42);
    }
  if (x < 0)
    return -1;
  return s42;  /* { dg-warning "uninitialized" } */
}
