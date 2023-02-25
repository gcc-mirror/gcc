
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>


int SocketControl_nonBlocking (int fd)
{
  return fcntl (fd, fcntl (fd, F_GETFL) | O_NONBLOCK);
}

int SocketControl_ignoreSignals (void)
{
  signal (SIGPIPE, SIG_IGN);
}

void _M2_SocketControl_init (int, char *, char *)
{
}

void _M2_SocketControl_finish (int, char *, char *)
{
}

void _M2_SocketControl_ctor ()
{
}
