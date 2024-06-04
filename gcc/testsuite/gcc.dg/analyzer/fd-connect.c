/* { dg-require-effective-target sockets } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-skip-if "PR analyzer/107750" { *-*-solaris2* } } */

#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <errno.h>
#include "analyzer-decls.h"

int test_connect (int sockfd, const struct sockaddr *addr,
		  socklen_t addrlen)
{
  return connect (sockfd, addr, addrlen);
}

void test_null_connect (int fd)
{
  errno = 0;
  int result = connect (fd, NULL, 0);
  __analyzer_eval (result == -1); /* { dg-warning "TRUE" } */
  __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
}

int test_uninit_addr (int fd, const char *sockname)
{
  struct sockaddr_un addr;
  return connect (fd, (struct sockaddr *)&addr, sizeof (addr));
  // TODO: complain about uninit addr.
}

void test_connect_after_bind (const char *sockname,
			      const struct sockaddr *baddr, socklen_t baddrlen,
			      const struct sockaddr *caddr, socklen_t caddrlen)
{
  int fd = socket (AF_UNIX, SOCK_STREAM, 0); /* { dg-message "stream socket created here" } */
  if (fd == -1)
    return;

  if (bind (fd, baddr, baddrlen) == -1)
    {
      close (fd);
      return;
    }

  connect (fd, caddr, caddrlen); /* { dg-warning "'connect' on file descriptor 'fd' in wrong phase" "warning" } */
  /* { dg-message "'connect' expects a new socket file descriptor but 'fd' is bound" "final event" { target *-*-* } .-1 } */

  close (fd);      
}

int test_connect_on_constant ()
{
  return connect (0, NULL, 0);
}
