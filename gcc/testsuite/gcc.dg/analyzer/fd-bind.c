/* { dg-require-effective-target sockets } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */

#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <errno.h>
#include "analyzer-decls.h"

void test_bind (int fd, const char *sockname)
{
  struct sockaddr_un addr;
  memset (&addr, 0, sizeof (addr));
  addr.sun_family = AF_UNIX;
  strncpy (addr.sun_path, sockname, sizeof(addr.sun_path) - 1);
  if (bind (fd, (struct sockaddr *)&addr, sizeof (addr)) == -1)
    __analyzer_dump_path (); /* { dg-message "path" } */
  else
    __analyzer_dump_path (); /* { dg-message "path" } */
}

void test_null_bind (int fd)
{
  errno = 0;
  int result = bind (fd, NULL, 0);
  __analyzer_eval (result == -1); /* { dg-warning "TRUE" } */
  __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
}

void test_double_bind (int fd, const char *sockname)
{
  struct sockaddr_un addr;
  memset (&addr, 0, sizeof (addr));
  addr.sun_family = AF_UNIX;
  strncpy (addr.sun_path, sockname, sizeof(addr.sun_path) - 1);
  bind (fd, (struct sockaddr *)&addr, sizeof (addr));
  bind (fd, (struct sockaddr *)&addr, sizeof (addr)); /* { dg-warning "'bind' on file descriptor 'fd' in wrong phase \\\[-Wanalyzer-fd-phase-mismatch\\\]" "warning" } */
  /* { dg-message "'bind' expects a new socket file descriptor but 'fd' has already been bound" "final event" { target *-*-* } .-1 } */
}

int test_uninit_addr (int fd, const char *sockname)
{
  struct sockaddr_un addr;
  return bind (fd, (struct sockaddr *)&addr, sizeof (addr));
  // TODO: complain about uninit addr.
}

void test_bind_after_connect (int fd, const char *sockname,
				const struct sockaddr *caddr, socklen_t caddrlen)
{
  if (connect (fd, caddr, caddrlen) == -1)
    return;

  struct sockaddr_un addr;
  memset (&addr, 0, sizeof (addr));
  addr.sun_family = AF_UNIX;
  strncpy (addr.sun_path, sockname, sizeof(addr.sun_path) - 1);
  bind (fd, (struct sockaddr *)&addr, sizeof (addr));
  /* TODO: we don't warn for this; after the plain "connect" we're
     in the stop state.  */
}

void test_bind_after_accept (int fd, const char *sockname)
{
  int afd = accept (fd, NULL, NULL);
  if (afd == -1)
    return;

  struct sockaddr_un addr;
  memset (&addr, 0, sizeof (addr));
  addr.sun_family = AF_UNIX;
  strncpy (addr.sun_path, sockname, sizeof(addr.sun_path) - 1);
  bind (afd, (struct sockaddr *)&addr, sizeof (addr)); /* { dg-warning "'bind' on file descriptor 'afd' in wrong phase \\\[-Wanalyzer-fd-phase-mismatch\\\]" "warning" } */
  /* { dg-message "'bind' expects a new socket file descriptor but 'afd' is already connected" "final event" { target *-*-* } .-1 } */

  close (afd);
}

int test_bind_on_constant ()
{
  return bind (0, NULL, 0);
}
