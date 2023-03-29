/* { dg-require-effective-target sockets } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */

#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <errno.h>
#include "analyzer-decls.h"

void test_active_open_from_scratch (const char *sockname, void *buf)
{
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

  struct sockaddr_un addr;
  memset (&addr, 0, sizeof (addr));
  addr.sun_family = AF_UNIX;
  strncpy (addr.sun_path, sockname, sizeof(addr.sun_path) - 1);

  errno = 0;
  if (connect (fd, (struct sockaddr *)&addr, sizeof (addr)) == -1)
    {
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-new-stream-socket'" } */
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      close (fd);
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
      return;
    }

  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-connected-stream-socket'" } */
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (fd >= 0); /* { dg-warning "TRUE" } */

  write (fd, "hello", 6);
  read (fd, buf, 100);

  close (fd);
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
}

void test_active_open_from_connect (int fd, const char *sockname, void *buf)
{
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'start'" } */

  struct sockaddr_un addr;
  memset (&addr, 0, sizeof (addr));
  addr.sun_family = AF_UNIX;
  strncpy (addr.sun_path, sockname, sizeof(addr.sun_path) - 1);

  errno = 0;
  if (connect (fd, (struct sockaddr *)&addr, sizeof (addr)) == -1)
    {
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-new-unknown-socket'" } */
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      close (fd);
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
      return;
    }

  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-stop'" } */
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (fd >= 0); /* { dg-warning "TRUE" } */

  write (fd, "hello", 6);
  read (fd, buf, 100);

  close (fd);
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-stop'" } */
}

void test_active_open_from_connect_constant (const char *sockname, void *buf)
{
  const int fd = 42;
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-constant'" } */

  struct sockaddr_un addr;
  memset (&addr, 0, sizeof (addr));
  addr.sun_family = AF_UNIX;
  strncpy (addr.sun_path, sockname, sizeof(addr.sun_path) - 1);

  errno = 0;
  if (connect (fd, (struct sockaddr *)&addr, sizeof (addr)) == -1)
    {
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-constant'" } */
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      close (fd);
      __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-closed'" } */
      return;
    }

  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-stop'" } */
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (fd >= 0); /* { dg-warning "TRUE" } */

  write (fd, "hello", 6);
  read (fd, buf, 100);

  close (fd);
  __analyzer_dump_state ("file-descriptor", fd); /* { dg-warning "state: 'fd-stop'" } */
}
