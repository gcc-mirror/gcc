/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               S O C K E T                                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 2003-2019, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/*  This file provides a portable binding to the sockets API                */

#define ATTRIBUTE_UNUSED __attribute__((unused))

/* Ensure access to errno is thread safe.  */
#ifndef _REENTRANT
#define _REENTRANT
#endif
#define _THREAD_SAFE

#include "gsocket.h"

#if defined (__FreeBSD__) || defined (__DragonFly__) \
 || defined (__NetBSD__) || defined (__OpenBSD__)
typedef unsigned int IOCTL_Req_T;
#else
typedef int IOCTL_Req_T;
#endif

#if defined(HAVE_SOCKETS)

/* Include all the necessary system-specific headers and define the
 * necessary macros (shared with gen-oscons).
 */

#if !defined(SO_NOSIGPIPE) && !defined (MSG_NOSIGNAL)
#include <signal.h>
#endif
/* Required if we will be calling signal() in __gnat_disable_all_sigpipes() */

#include "raise.h"
/* Required for __gnat_malloc() */

#include <string.h>
/* Required for memcpy() */

extern void __gnat_disable_sigpipe (int fd);
extern void __gnat_disable_all_sigpipes (void);
extern int  __gnat_create_signalling_fds (int *fds);
extern int  __gnat_read_signalling_fd (int rsig);
extern int  __gnat_write_signalling_fd (int wsig);
extern void  __gnat_close_signalling_fd (int sig);
extern void __gnat_last_socket_in_set (fd_set *, int *);
extern void __gnat_get_socket_from_set (fd_set *, int *, int *);
extern void __gnat_insert_socket_in_set (fd_set *, int);
extern int __gnat_is_socket_in_set (fd_set *, int);
extern fd_set *__gnat_new_socket_set (fd_set *);
extern void __gnat_remove_socket_from_set (fd_set *, int);
extern void __gnat_reset_socket_set (fd_set *);
extern int  __gnat_get_h_errno (void);
extern int  __gnat_socket_ioctl (int, IOCTL_Req_T, int *);

extern char * __gnat_servent_s_name (struct servent *);
extern char * __gnat_servent_s_alias (struct servent *, int index);
extern unsigned short __gnat_servent_s_port (struct servent *);
extern char * __gnat_servent_s_proto (struct servent *);

extern char * __gnat_hostent_h_name (struct hostent *);
extern char * __gnat_hostent_h_alias (struct hostent *, int);
extern int __gnat_hostent_h_addrtype (struct hostent *);
extern int __gnat_hostent_h_length (struct hostent *);
extern char * __gnat_hostent_h_addr (struct hostent *, int);

extern int __gnat_getaddrinfo(
  const char *node,
  const char *service,
  const struct addrinfo *hints,
  struct addrinfo **res);
int __gnat_getnameinfo(
  const struct sockaddr *sa, socklen_t salen,
  char *host, size_t hostlen,
  char *serv, size_t servlen, int flags);
extern void __gnat_freeaddrinfo(struct addrinfo *res);
extern const char * __gnat_gai_strerror(int errcode);

#ifndef HAVE_INET_PTON
extern int  __gnat_inet_pton (int, const char *, void *);
#endif

#ifndef HAVE_INET_NTOP
extern const char *
__gnat_inet_ntop(int, const void *, char *, socklen_t);
#endif

/* Disable the sending of SIGPIPE for writes on a broken stream */

void
__gnat_disable_sigpipe (int fd ATTRIBUTE_UNUSED)
{
#ifdef SO_NOSIGPIPE
  int val = 1;
  (void) setsockopt (fd, SOL_SOCKET, SO_NOSIGPIPE, &val, sizeof val);
#endif
}

void
__gnat_disable_all_sigpipes (void)
{
#if !defined(SO_NOSIGPIPE) && !defined(MSG_NOSIGNAL) && defined(SIGPIPE)
  (void) signal (SIGPIPE, SIG_IGN);
#endif
}

#if defined (_WIN32) || defined (__vxworks)
/*
 * Signalling FDs operations are implemented in Ada for these platforms
 * (see subunit GNAT.Sockets.Thin.Signalling_Fds).
 */
#else
/*
 * Create a pair of connected file descriptors fds[0] and fds[1] used for
 * signalling by a Selector object. fds[0] is the read end, and fds[1] the
 * write end.
 */
int
__gnat_create_signalling_fds (int *fds) {
  return pipe (fds);
}

/*
 * Read one byte of data from rsig, the read end of a pair of signalling fds
 * created by __gnat_create_signalling_fds.
 */
int
__gnat_read_signalling_fd (int rsig) {
  char c;
  return read (rsig, &c, 1);
}

/*
 * Write one byte of data to wsig, the write end of a pair of signalling fds
 * created by __gnat_create_signalling_fds.
 */
int
__gnat_write_signalling_fd (int wsig) {
  char c = 0;
  return write (wsig, &c, 1);
}

/*
 * Close one end of a pair of signalling fds
 */
void
__gnat_close_signalling_fd (int sig) {
  (void) close (sig);
}
#endif

/*
 * Handling of gethostbyname, gethostbyaddr, getservbyname and getservbyport
 * =========================================================================
 *
 * This module exposes __gnat_getXXXbyYYY operations with the same signature
 * as the reentrant variant getXXXbyYYY_r.
 *
 * On platforms where getXXXbyYYY is intrinsically reentrant, the provided user
 * buffer argument is ignored.
 *
 * When getXXXbyYYY is not reentrant but getXXXbyYYY_r exists, the latter is
 * used, and the provided buffer argument must point to a valid, thread-local
 * buffer (usually on the caller's stack).
 *
 * When getXXXbyYYY is not reentrant and no reentrant getXXXbyYYY_r variant
 * is available, the non-reentrant getXXXbyYYY is called, the provided user
 * buffer is ignored, and the caller is expected to take care of mutual
 * exclusion.
 */

#ifdef HAVE_GETxxxBYyyy_R
int
__gnat_gethostbyname (const char *name,
  struct hostent *ret, char *buf, size_t buflen,
  int *h_errnop)
{
  struct hostent *rh;
  int ri;

#if defined(__linux__) || defined(__GLIBC__) || defined(__rtems__)
  (void) gethostbyname_r (name, ret, buf, buflen, &rh, h_errnop);
#else
  rh = gethostbyname_r (name, ret, buf, buflen, h_errnop);
#endif
  ri = (rh == NULL) ? -1 : 0;
  return ri;
}

int
__gnat_gethostbyaddr (const char *addr, int len, int type,
  struct hostent *ret, char *buf, size_t buflen,
  int *h_errnop)
{
  struct hostent *rh;
  int ri;

#if defined(__linux__) || defined(__GLIBC__) || defined(__rtems__)
  (void) gethostbyaddr_r (addr, len, type, ret, buf, buflen, &rh, h_errnop);
#else
  rh = gethostbyaddr_r (addr, len, type, ret, buf, buflen, h_errnop);
#endif
  ri = (rh == NULL) ? -1 : 0;
  return ri;
}

int
__gnat_getservbyname (const char *name, const char *proto,
  struct servent *ret, char *buf, size_t buflen)
{
  struct servent *rh;
  int ri;

#if defined(__linux__) || defined(__GLIBC__) || defined(__rtems__)
  (void) getservbyname_r (name, proto, ret, buf, buflen, &rh);
#else
  rh = getservbyname_r (name, proto, ret, buf, buflen);
#endif
  ri = (rh == NULL) ? -1 : 0;
  return ri;
}

int
__gnat_getservbyport (int port, const char *proto,
  struct servent *ret, char *buf, size_t buflen)
{
  struct servent *rh;
  int ri;

#if defined(__linux__) || defined(__GLIBC__) || defined(__rtems__)
  (void) getservbyport_r (port, proto, ret, buf, buflen, &rh);
#else
  rh = getservbyport_r (port, proto, ret, buf, buflen);
#endif
  ri = (rh == NULL) ? -1 : 0;
  return ri;
}
#elif defined (__vxworks)
static char vxw_h_name[MAXHOSTNAMELEN + 1];
static char *vxw_h_aliases[1] = { NULL };
static int vxw_h_addr;
static char *vxw_h_addr_list[2] = { (char*) &vxw_h_addr, NULL };

int
__gnat_gethostbyname (const char *name,
  struct hostent *ret, char *buf, size_t buflen,
  int *h_errnop)
{
  vxw_h_addr = hostGetByName (name);
  if (vxw_h_addr == ERROR) {
    *h_errnop = __gnat_get_h_errno ();
    return -1;
  }
  ret->h_name      = name;
  ret->h_aliases   = &vxw_h_aliases;
  ret->h_addrtype  = AF_INET;
  ret->h_length    = 4;
  ret->h_addr_list = &vxw_h_addr_list;
  return 0;
}

int
__gnat_gethostbyaddr (const char *addr, int len, int type,
  struct hostent *ret, char *buf, size_t buflen,
  int *h_errnop)
{
  if (type != AF_INET) {
    *h_errnop = EAFNOSUPPORT;
    return -1;
  }

  if (addr == NULL || len != 4) {
    *h_errnop = EINVAL;
    return -1;
  }

  if (hostGetByAddr (*(int*)addr, &vxw_h_name) != OK) {
    *h_errnop = __gnat_get_h_errno ();
    return -1;
  }

  vxw_h_addr       = addr;

  ret->h_name      = &vxw_h_name;
  ret->h_aliases   = &vxw_h_aliases;
  ret->h_addrtype  = AF_INET;
  ret->h_length    = 4;
  ret->h_addr_list = &vxw_h_addr_list;
}

int
__gnat_getservbyname (const char *name, const char *proto,
  struct servent *ret, char *buf, size_t buflen)
{
  /* Not available under VxWorks */
  return -1;
}

int
__gnat_getservbyport (int port, const char *proto,
  struct servent *ret, char *buf, size_t buflen)
{
  /* Not available under VxWorks */
  return -1;
}
#else
int
__gnat_gethostbyname (const char *name,
  struct hostent *ret, char *buf, size_t buflen,
  int *h_errnop)
{
  struct hostent *rh;
  rh = gethostbyname (name);
  if (rh == NULL) {
    *h_errnop = __gnat_get_h_errno ();
    return -1;
  }
  *ret = *rh;
  *h_errnop = 0;
  return 0;
}

int
__gnat_gethostbyaddr (const char *addr, int len, int type,
  struct hostent *ret, char *buf, size_t buflen,
  int *h_errnop)
{
  struct hostent *rh;
  rh = gethostbyaddr (addr, len, type);
  if (rh == NULL) {
    *h_errnop = __gnat_get_h_errno ();
    return -1;
  }
  *ret = *rh;
  *h_errnop = 0;
  return 0;
}

int
__gnat_getservbyname (const char *name, const char *proto,
  struct servent *ret, char *buf, size_t buflen)
{
  struct servent *rh;
  rh = getservbyname (name, proto);
  if (rh == NULL)
    return -1;
  *ret = *rh;
  return 0;
}

int
__gnat_getservbyport (int port, const char *proto,
  struct servent *ret, char *buf, size_t buflen)
{
  struct servent *rh;
  rh = getservbyport (port, proto);
  if (rh == NULL)
    return -1;
  *ret = *rh;
  return 0;
}
#endif

/* Find the largest socket in the socket set SET. This is needed for
   `select'.  LAST is the maximum value for the largest socket. This hint is
   used to avoid scanning very large socket sets.  On return, LAST is the
   actual largest socket in the socket set. */

void
__gnat_last_socket_in_set (fd_set *set, int *last)
{
  int s;
  int l;
  l = -1;

#ifdef _WIN32
  /* More efficient method for NT. */
  for (s = 0; s < set->fd_count; s++)
    if ((int) set->fd_array[s] > l)
      l = set->fd_array[s];

#else

  for (s = *last; s != -1; s--)
    if (FD_ISSET (s, set))
      {
	l = s;
	break;
      }
#endif

  *last = l;
}

/* Get last socket and remove it from the socket set SET.  LAST is the
   maximum value of the largest socket.  This hint is used to avoid scanning
   very large socket sets.  On return, LAST is set to the actual largest
   socket in the socket set. */

void
__gnat_get_socket_from_set (fd_set *set, int *last, int *socket)
{
  *socket = *last;
  FD_CLR (*socket, set);
  __gnat_last_socket_in_set (set, last);
}

/* Insert SOCKET in the socket set SET. */

void
__gnat_insert_socket_in_set (fd_set *set, int socket)
{
  FD_SET (socket, set);
}

/* Check whether a given SOCKET is in the socket set SET. */

int
__gnat_is_socket_in_set (fd_set *set, int socket)
{
  return FD_ISSET (socket, set);
}

/* Remove SOCKET from the socket set SET. */

void
__gnat_remove_socket_from_set (fd_set *set, int socket)
{
  FD_CLR (socket, set);
}

/* Reset SET */
void
__gnat_reset_socket_set (fd_set *set)
{
  FD_ZERO (set);
}

/* Get the value of the last host error */

int
__gnat_get_h_errno (void) {
#ifdef __vxworks
  int vxw_errno = errno;

  switch (vxw_errno) {
    case 0:
      return 0;

#ifdef S_hostLib_HOST_NOT_FOUND
    case S_hostLib_HOST_NOT_FOUND:
#endif
    case S_hostLib_UNKNOWN_HOST:
      return HOST_NOT_FOUND;

#ifdef S_hostLib_TRY_AGAIN
    case S_hostLib_TRY_AGAIN:
      return TRY_AGAIN;
#endif

#ifdef S_hostLib_NO_RECOVERY
    case S_hostLib_NO_RECOVERY:
#endif
#ifdef S_hostLib_NETDB_INTERNAL
    case S_hostLib_NETDB_INTERNAL:
#endif
    case S_hostLib_INVALID_PARAMETER:
      return NO_RECOVERY;

    default:
      return -1;
  }

#elif defined (__rtems__)
  /* At this stage in the tool build, no networking .h files are available.
   * Newlib does not provide networking .h files and RTEMS is not built yet.
   * So we need to explicitly extern h_errno to access it.
   */
  extern int h_errno;
  return h_errno;

#else
  return h_errno;
#endif
}

/* Wrapper for ioctl(2), which is a variadic function */

int
__gnat_socket_ioctl (int fd, IOCTL_Req_T req, int *arg) {
#if defined (_WIN32)
  return ioctlsocket (fd, req, arg);
#elif defined (__APPLE__)
  /*
   * On Darwin, req is an unsigned long, and we want to convert without sign
   * extension to get the proper bit pattern in the case of a 64 bit kernel.
   */
  return ioctl (fd, (unsigned int) req, arg);
#else
  return ioctl (fd, req, arg);
#endif
}

#ifndef HAVE_INET_PTON

int
__gnat_inet_pton (int af, const char *src, void *dst) {
  switch (af) {
#if defined (_WIN32) && defined (AF_INET6)
    case AF_INET6:
#endif
    case AF_INET:
      break;
    default:
      errno = EAFNOSUPPORT;
      return -1;
  }

#if defined (__vxworks)
  return (inet_aton (src, dst) == OK);

#elif defined (_WIN32)
  struct sockaddr_storage ss;
  int sslen = sizeof ss;
  int rc;

  ss.ss_family = af;
  rc = WSAStringToAddressA (src, af, NULL, (struct sockaddr *)&ss, &sslen);
  if (rc == 0) {
    switch (af) {
      case AF_INET:
        *(struct in_addr *)dst = ((struct sockaddr_in *)&ss)->sin_addr;
        break;
#ifdef AF_INET6
      case AF_INET6:
        *(struct in6_addr *)dst = ((struct sockaddr_in6 *)&ss)->sin6_addr;
        break;
#endif
    }
  }
  return (rc == 0);

#elif defined (__hpux__)
  in_addr_t addr;
  int rc = -1;

  if (src == NULL || dst == NULL) {
    errno = EINVAL;

  } else if (!strcmp (src, "255.255.255.255")) {
    addr = 0xffffffff;
    rc = 1;

  } else {
    addr = inet_addr (src);
    rc = (addr != 0xffffffff);
  }
  if (rc == 1) {
    *(in_addr_t *)dst = addr;
  }
  return rc;
#endif
}
#endif

#ifndef HAVE_INET_NTOP

const char *
__gnat_inet_ntop(int af, const void *src, char *dst, socklen_t size)
{
#ifdef _WIN32
  struct sockaddr_storage ss;
  int sslen = sizeof ss;
  memset(&ss, 0, sslen);
  ss.ss_family = af;

  switch (af) {
    case AF_INET6:
      ((struct sockaddr_in6 *)&ss)->sin6_addr = *(struct in6_addr *)src;
      break;
    case AF_INET:
      ((struct sockaddr_in *)&ss)->sin_addr = *(struct in_addr *)src;
      break;
    default:
      errno = EAFNOSUPPORT;
      return NULL;
  }

  DWORD sz = size;

  if (WSAAddressToStringA((struct sockaddr*)&ss, sslen, 0, dst, &sz) != 0) {
     return NULL;
  }
  return dst;
#else
  return NULL;
#endif
}
#endif

/*
 * Accessor functions for struct hostent.
 */

char * __gnat_hostent_h_name (struct hostent * h) {
  return h->h_name;
}

char * __gnat_hostent_h_alias (struct hostent * h, int index) {
  return h->h_aliases[index];
}

int __gnat_hostent_h_addrtype (struct hostent * h) {
  return h->h_addrtype;
}

int __gnat_hostent_h_length (struct hostent * h) {
  return h->h_length;
}

char * __gnat_hostent_h_addr (struct hostent * h, int index) {
  return h->h_addr_list[index];
}

/*
 * Accessor functions for struct servent.
 *
 * These are needed because servent has different representations on different
 * platforms, and we don't want to deal with that on the Ada side. For example,
 * on Linux, we have (see /usr/include netdb.h):
 *
 *   struct servent
 *   {
 *     char *s_name;
 *     char **s_aliases;
 *     int s_port;
 *     char *s_proto;
 *   };
 *
 * and on Windows (see mingw's socket.h):
 *
 *   struct servent {
 *     char *s_name;
 *     char **s_aliases;
 *   #ifdef _WIN64
 *     char *s_proto;
 *     short s_port;
 *   #else
 *     short s_port;
 *     char *s_proto;
 *   #endif
 *   };
 */

char *
__gnat_servent_s_name (struct servent * s)
{
  return s->s_name;
}

char *
__gnat_servent_s_alias (struct servent * s, int index)
{
  return s->s_aliases[index];
}

unsigned short
__gnat_servent_s_port (struct servent * s)
{
  return s->s_port;
}

char *
__gnat_servent_s_proto (struct servent * s)
{
  return s->s_proto;
}

#if defined(AF_INET6) && !defined(__rtems__)

int __gnat_getaddrinfo(
  const char *node,
  const char *service,
  const struct addrinfo *hints,
  struct addrinfo **res)
{
  return getaddrinfo(node, service, hints, res);
}

int __gnat_getnameinfo(
  const struct sockaddr *sa, socklen_t salen,
  char *host, size_t hostlen,
  char *serv, size_t servlen, int flags)
{
  return getnameinfo(sa, salen, host, hostlen, serv, servlen, flags);
}

void __gnat_freeaddrinfo(struct addrinfo *res) {
   freeaddrinfo(res);
}

const char * __gnat_gai_strerror(int errcode) {
#if defined(_WIN32) ||  defined(__vxworks)
  // gai_strerror thread usafe on Windows and is not available on some vxWorks
  // versions

  switch (errcode) {
    case EAI_AGAIN:
      return "Temporary failure in name resolution.";
    case EAI_BADFLAGS:
      return "Invalid value for ai_flags.";
    case EAI_FAIL:
      return "Nonrecoverable failure in name resolution.";
    case EAI_FAMILY:
      return "The ai_family member is not supported.";
    case EAI_MEMORY:
      return "Memory allocation failure.";
#ifdef EAI_NODATA
    // Could be not defined under the vxWorks
    case EAI_NODATA:
      return "No address associated with nodename.";
#endif
#if EAI_NODATA != EAI_NONAME
    /* with mingw64 runtime EAI_NODATA and EAI_NONAME have the same value.
       This applies to both win32 and win64 */
    case EAI_NONAME:
      return "Neither nodename nor servname provided, or not known.";
#endif
    case EAI_SERVICE:
      return "The servname parameter is not supported for ai_socktype.";
    case EAI_SOCKTYPE:
      return "The ai_socktype member is not supported.";
#ifdef EAI_SYSTEM
    // Could be not defined, at least on Windows
    case EAI_SYSTEM:
      return "System error returned in errno";
#endif
    default:
      return "Unknown error.";
    }
#else
   return gai_strerror(errcode);
#endif
}

#else

int __gnat_getaddrinfo(
  const char *node,
  const char *service,
  const struct addrinfo *hints,
  struct addrinfo **res)
{
  return -1;
}

int __gnat_getnameinfo(
  const struct sockaddr *sa, socklen_t salen,
  char *host, size_t hostlen,
  char *serv, size_t servlen, int flags)
{
  return -1;
}

void __gnat_freeaddrinfo(struct addrinfo *res) {
}

const char * __gnat_gai_strerror(int errcode) {
   return "getaddinfo functions family is not supported";
}

#endif

int __gnat_minus_500ms() {
#if defined (_WIN32)
  // Windows Server 2019 and Windows 8.0 do not need 500 millisecond socket
  // timeout correction.
  return !(IsWindows8OrGreater() && !IsWindowsServer()
           || IsWindowsVersionOrGreater(10, 0, 17763));
#else
   return 0;
#endif
}

#endif /* defined(HAVE_SOCKETS) */
