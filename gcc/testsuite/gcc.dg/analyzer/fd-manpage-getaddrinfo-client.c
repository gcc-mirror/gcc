/* Example from getaddrinfo.3 manpage, which has this license:

Copyright (c) 2007, 2008 Michael Kerrisk <mtk.manpages@gmail.com>
and Copyright (c) 2006 Ulrich Drepper <drepper@redhat.com>
A few pieces of an earlier version remain:
Copyright 2000, Sam Varshavchik <mrsam@courier-mta.com>

Permission is granted to make and distribute verbatim copies of this
manual provided the copyright notice and this permission notice are
preserved on all copies.

Permission is granted to copy and distribute modified versions of this
manual under the conditions for verbatim copying, provided that the
entire resulting derived work is distributed under the terms of a
permission notice identical to this one.

Since the Linux kernel and libraries are constantly changing, this
manual page may be incorrect or out-of-date.  The author(s) assume no
responsibility for errors or omissions, or for damages resulting from
the use of the information contained herein.  The author(s) may not
have taken the same level of care in the production of this manual,
which is licensed free of charge, as they might when working
professionally.

Formatted or processed versions of this manual, if unaccompanied by
the source, must acknowledge the copyright and authors of this work.
*/

/* { dg-require-effective-target sockets } */
/* { dg-additional-options "-Wno-analyzer-too-complex" } */

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#define BUF_SIZE 500

int
main(int argc, char *argv[])
{
  struct addrinfo hints;
  struct addrinfo *result, *rp;
  int sfd, s;
  size_t len;
  ssize_t nread;
  char buf[BUF_SIZE];

  if (argc < 3) {
    fprintf(stderr, "Usage: %s host port msg...\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  /* Obtain address(es) matching host/port. */

  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_UNSPEC;    /* Allow IPv4 or IPv6 */
  hints.ai_socktype = SOCK_DGRAM; /* Datagram socket */
  hints.ai_flags = 0;
  hints.ai_protocol = 0;          /* Any protocol */

  s = getaddrinfo(argv[1], argv[2], &hints, &result);
  if (s != 0) {
    fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(s));
    exit(EXIT_FAILURE);
  }

  /* getaddrinfo() returns a list of address structures.
     Try each address until we successfully connect(2).
     If socket(2) (or connect(2)) fails, we (close the socket
     and) try the next address. */

  for (rp = result; rp != NULL; rp = rp->ai_next) {
    sfd = socket(rp->ai_family, rp->ai_socktype,
		 rp->ai_protocol);
    if (sfd == -1)
      continue;

    if (connect(sfd, rp->ai_addr, rp->ai_addrlen) != -1)
      break;                  /* Success */

    close(sfd);
  }

  freeaddrinfo(result);           /* No longer needed */

  if (rp == NULL) {               /* No address succeeded */
    fprintf(stderr, "Could not connect\n");
    exit(EXIT_FAILURE);
  }

  /* Send remaining command-line arguments as separate
     datagrams, and read responses from server. */

  for (int j = 3; j < argc; j++) {
    len = strlen(argv[j]) + 1;
    /* +1 for terminating null byte */

    if (len > BUF_SIZE) {
      fprintf(stderr,
	      "Ignoring long message in argument %d\n", j);
      continue;
    }

    if (write(sfd, argv[j], len) != len) {
      fprintf(stderr, "partial/failed write\n");
      exit(EXIT_FAILURE);
    }

    nread = read(sfd, buf, BUF_SIZE);
    if (nread == -1) {
      perror("read");
      exit(EXIT_FAILURE);
    }

    printf("Received %zd bytes: %s\n", nread, buf);
  }

  exit(EXIT_SUCCESS);
}
