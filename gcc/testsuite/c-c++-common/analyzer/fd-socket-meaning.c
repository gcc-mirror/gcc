/* { dg-require-effective-target sockets } */

/* Needed on some targets until we have exception-handling working (PR 111475).  */
/* { dg-additional-options "-fno-exceptions" } */

/* { dg-additional-options "-fanalyzer-verbose-state-changes" } */

#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <errno.h>

void test_leak_unchecked_stream_socket (void)
{
  int fd = socket (AF_UNIX, SOCK_STREAM, 0); /* { dg-message "meaning: \\{verb: 'acquire', noun: 'resource'\\}" } */
} /* { dg-warning "leak of file descriptor 'fd'" } */

void test_leak_unchecked_datagram_socket (void)
{
  int fd = socket (AF_UNIX, SOCK_DGRAM, 0); /* { dg-message "meaning: \\{verb: 'acquire', noun: 'resource'\\}" } */
} /* { dg-warning "leak of file descriptor 'fd'" } */

void test_leak_unchecked_socket (int type)
{
  int fd = socket (AF_UNIX, type, 0); /* { dg-message "meaning: \\{verb: 'acquire', noun: 'resource'\\}" } */
} /* { dg-warning "leak of file descriptor 'fd'" } */
