/* Verify the various states when performing a passive open,
   either from scratch, or when various phases are assumed to already
   be done.  */

/* { dg-require-effective-target sockets } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-skip-if "PR analyzer/107750" { *-*-solaris2* } } */

#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <errno.h>
#include "analyzer-decls.h"

void test_passive_open_from_scratch (const char *sockname, void *buf)
{
  struct sockaddr_un addr;
  int afd;
  errno = 0;
  int fd = socket (AF_UNIX, SOCK_STREAM, 0);
  if (fd == -1)
    {
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-invalid'" } */
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      return;
    }
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-new-stream-socket'" } */
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  memset (&addr, 0, sizeof (addr));
  addr.sun_family = AF_UNIX;
  strncpy (addr.sun_path, sockname, sizeof(addr.sun_path) - 1);
  errno = 0;
  if (bind (fd, (struct sockaddr *)&addr, sizeof (addr)) == -1)
    {
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-new-stream-socket'" } */
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      close (fd);
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
      return;
    }
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-bound-stream-socket'" } */
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (fd >= 0); /* { dg-warning "TRUE" } */
  if (listen (fd, 5) == -1)
    {
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-bound-stream-socket'" } */
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      close (fd);
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
      return;
    }
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-listening-stream-socket'" } */
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (fd >= 0); /* { dg-warning "TRUE" } */
  afd = accept (fd, NULL, NULL);
  if (afd == -1)
    {
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-listening-stream-socket'" } */
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      close (fd);
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
      return;
    }
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-listening-stream-socket'" } */
  __analyzer_dump_state ("file-descriptor", afd); /* { dg-warning "state: 'fd-connected-stream-socket'" } */
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (fd >= 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (afd >= 0); /* { dg-warning "TRUE" } */

  write (afd, "hello", 6);
  read (afd, buf, 100);
  
  close (afd);
  close (fd);
  __analyzer_dump_state ("file-descriptor", afd); /* { dg-warning "state: 'fd-closed'" } */
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
}

void test_passive_open_from_bind (int fd, const char *sockname, void *buf)
{
  struct sockaddr_un addr;
  int afd;
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'start'" } */
  memset (&addr, 0, sizeof (addr));
  addr.sun_family = AF_UNIX;
  strncpy (addr.sun_path, sockname, sizeof(addr.sun_path) - 1);
  errno = 0;
  if (bind (fd, (struct sockaddr *)&addr, sizeof (addr)) == -1)
    {
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-new-unknown-socket'" } */
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      close (fd);
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
      return;
    }
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-bound-unknown-socket'" } */
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (fd >= 0); /* { dg-warning "TRUE" } */
  if (listen (fd, 5) == -1)
    {
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-bound-unknown-socket'" } */
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      close (fd);
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
      return;
    }
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-listening-stream-socket'" } */
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (fd >= 0); /* { dg-warning "TRUE" } */
  afd = accept (fd, NULL, NULL);
  if (afd == -1)
    {
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-listening-stream-socket'" } */
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      close (fd);
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
      return;
    }
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-listening-stream-socket'" } */
  __analyzer_dump_state ("file-descriptor", afd); /* { dg-warning "state: 'fd-connected-stream-socket'" } */
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (fd >= 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (afd >= 0); /* { dg-warning "TRUE" } */

  write (afd, "hello", 6);  
  read (afd, buf, 100);

  close (afd);
  close (fd);
  __analyzer_dump_state ("file-descriptor", afd); /* { dg-warning "state: 'fd-closed'" } */
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
}

void test_passive_open_from_bind_constant (const char *sockname, void *buf)
{
  const int fd = 42;
  struct sockaddr_un addr;
  int afd;
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-constant'" } */
  memset (&addr, 0, sizeof (addr));
  addr.sun_family = AF_UNIX;
  strncpy (addr.sun_path, sockname, sizeof(addr.sun_path) - 1);
  errno = 0;
  if (bind (fd, (struct sockaddr *)&addr, sizeof (addr)) == -1)
    {
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-constant'" } */
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      close (fd);
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
      return;
    }
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-bound-unknown-socket'" } */
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (fd >= 0); /* { dg-warning "TRUE" } */
  if (listen (fd, 5) == -1)
    {
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-bound-unknown-socket'" } */
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      close (fd);
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
      return;
    }
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-listening-stream-socket'" } */
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (fd >= 0); /* { dg-warning "TRUE" } */
  afd = accept (fd, NULL, NULL);
  if (afd == -1)
    {
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-listening-stream-socket'" } */
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      close (fd);
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
      return;
    }
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-listening-stream-socket'" } */
  __analyzer_dump_state ("file-descriptor", afd); /* { dg-warning "state: 'fd-connected-stream-socket'" } */
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (fd >= 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (afd >= 0); /* { dg-warning "TRUE" } */

  write (afd, "hello", 6);  
  read (afd, buf, 100);

  close (afd);
  close (fd);
  __analyzer_dump_state ("file-descriptor", afd); /* { dg-warning "state: 'fd-closed'" } */
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
}

void test_passive_open_from_listen (int fd, void *buf)
{
  int afd;
  errno = 0;
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'start'" } */
  if (listen (fd, 5) == -1)
    {
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-bound-stream-socket'" } */
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      close (fd);
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
      return;
    }
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-listening-stream-socket'" } */
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (fd >= 0); /* { dg-warning "TRUE" } */
  afd = accept (fd, NULL, NULL);
  if (afd == -1)
    {
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-listening-stream-socket'" } */
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      close (fd);
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
      return;
    }
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-listening-stream-socket'" } */
  __analyzer_dump_state ("file-descriptor", afd); /* { dg-warning "state: 'fd-connected-stream-socket'" } */
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (fd >= 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (afd >= 0); /* { dg-warning "TRUE" } */

  write (afd, "hello", 6);
  read (afd, buf, 100);

  close (afd);
  close (fd);
  __analyzer_dump_state ("file-descriptor", afd); /* { dg-warning "state: 'fd-closed'" } */
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
}


void test_passive_open_from_listen_constant (void *buf)
{
  const int fd = 42;
  int afd;
  errno = 0;
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-constant'" } */
  if (listen (fd, 5) == -1)
    {
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-constant'" } */
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      close (fd);
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
      return;
    }
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-listening-stream-socket'" } */
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (fd >= 0); /* { dg-warning "TRUE" } */
  afd = accept (fd, NULL, NULL);
  if (afd == -1)
    {
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-listening-stream-socket'" } */
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      close (fd);
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
      return;
    }
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-listening-stream-socket'" } */
  __analyzer_dump_state ("file-descriptor", afd); /* { dg-warning "state: 'fd-connected-stream-socket'" } */
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (fd >= 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (afd >= 0); /* { dg-warning "TRUE" } */

  write (afd, "hello", 6);
  read (afd, buf, 100);

  close (afd);
  close (fd);
  __analyzer_dump_state ("file-descriptor", afd); /* { dg-warning "state: 'fd-closed'" } */
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
}

void test_passive_open_from_accept (int fd, void *buf)
{
  int afd;
  errno = 0;
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'start'" } */
  afd = accept (fd, NULL, NULL);
  if (afd == -1)
    {
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-listening-stream-socket'" } */
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      close (fd);
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
      return;
    }
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-listening-stream-socket'" } */
  __analyzer_dump_state ("file-descriptor", afd); /* { dg-warning "state: 'fd-connected-stream-socket'" } */
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (fd >= 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (afd >= 0); /* { dg-warning "TRUE" } */

  write (afd, "hello", 6);
  read (afd, buf, 100);

  close (afd);
  close (fd);
  __analyzer_dump_state ("file-descriptor", afd); /* { dg-warning "state: 'fd-closed'" } */
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
}
