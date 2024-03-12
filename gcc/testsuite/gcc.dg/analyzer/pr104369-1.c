/* { dg-additional-options "-Wno-analyzer-too-complex -Wno-analyzer-fd-leak" } */
// TODO: remove need for these options
/* C only: C++ does not support transparent_union. */


typedef __SIZE_TYPE__ size_t;
#define NULL ((void *)0)
#define POLLIN 0x001

typedef struct {
  unsigned long int __val[(1024 / (8 * sizeof(unsigned long int)))];
} __sigset_t;

typedef unsigned int socklen_t;

struct timespec {
  /* [...snip...] */
};

typedef unsigned long int nfds_t;

struct pollfd {
  int fd;
  short int events;
  short int revents;
};

extern int ppoll(struct pollfd *__fds, nfds_t __nfds,
                 const struct timespec *__timeout, const __sigset_t *__ss);

struct sockaddr_un {
  /* [...snip...] */
  char sun_path[108];
};

typedef union {
  /* [...snip...] */
  struct sockaddr_un *__restrict __sockaddr_un__;
  /* [...snip...] */
} __SOCKADDR_ARG __attribute__((transparent_union));

extern int accept(int __fd, __SOCKADDR_ARG __addr,
                  socklen_t *__restrict __addr_len);

extern void *calloc(size_t __nmemb, size_t __size)
  __attribute__((__nothrow__, __leaf__))
  __attribute__((__malloc__))
  __attribute__((__alloc_size__(1, 2)));

extern void *realloc(void *__ptr, size_t __size)
  __attribute__((__nothrow__, __leaf__))
  __attribute__((__warn_unused_result__))
  __attribute__((__alloc_size__(2)));

extern void exit(int __status)
  __attribute__((__nothrow__, __leaf__))
  __attribute__((__noreturn__));

int main() {
  int rc;
  int nsockets = 1;
  struct pollfd *pollfds, *newpollfds;
  struct sockaddr_un remote;
  socklen_t len = sizeof(remote);

  pollfds = (struct pollfd *) calloc(1, sizeof(struct pollfd));
  if (!pollfds) {
    exit(1);
  }

  while (1) {
    rc = ppoll(pollfds, nsockets, NULL, NULL);
    if (rc < 0) {
      continue;
    }

    if (pollfds[0].revents & POLLIN) {
      nsockets++;
      newpollfds = (struct pollfd *) realloc(pollfds, nsockets * sizeof(*pollfds));
      if (!newpollfds) {
        exit(1);
      }
      pollfds = newpollfds;
      pollfds[nsockets - 1].fd = accept(pollfds[0].fd, &remote, &len);
      /* { dg-error "could not convert '& remote' from 'sockaddr_un*' to '__SOCKADDR_ARG'" "G++ doesn't support transparent_union" { target c++ } .-1 } */
    }
  }
  return 0;
}
