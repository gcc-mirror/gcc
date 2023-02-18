/* Reduced from qemu-7.2.0's tests/qtest/libqtest.c.  */

#define	EINTR 4

#define g_assert_cmpint(n1, cmp, n2)					\
  do {									\
    gint64 __n1 = (n1), __n2 = (n2);					\
    if (__n1 cmp __n2) ; else						\
      g_assertion_message_cmpnum ("", __FILE__, __LINE__, __func__, \
				  #n1 " " #cmp " " #n2, (long double) __n1, #cmp, (long double) __n2, 'i'); \
  } while (0)

typedef __SIZE_TYPE__ size_t;
typedef unsigned int __socklen_t;
extern int snprintf (char *__restrict __s, size_t __maxlen,
       const char *__restrict __format, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 4)));
typedef __socklen_t socklen_t;
extern int *__errno_location (void) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));
#define errno (*__errno_location ())
typedef signed long gint64;
typedef char gchar;
extern
void g_assertion_message_cmpnum (const char *domain,
				 const char *file,
				 int line,
				 const char *func,
				 const char *expr,
				 long double arg1,
				 const char *cmp,
				 long double arg2,
				 char numtype);
enum __socket_type
{
  SOCK_STREAM = 1,
  /* [...snip...] */
};

typedef unsigned short int sa_family_t;

typedef union {
  const struct sockaddr *__restrict __sockaddr__;
  /* [...snip...] */
} __CONST_SOCKADDR_ARG __attribute__ ((__transparent_union__));

extern int socket (int __domain, int __type, int __protocol)
  __attribute__ ((__nothrow__ , __leaf__));
extern int bind (int __fd, __CONST_SOCKADDR_ARG __addr, socklen_t __len)
  __attribute__ ((__nothrow__ , __leaf__));
extern int listen (int __fd, int __n)
  __attribute__ ((__nothrow__ , __leaf__));

struct sockaddr_un
{
  sa_family_t sun_family;
  char sun_path[108];
};

int qtest_socket_server(const char *socket_path)
{
    struct sockaddr_un addr;
    int sock;
    int ret;

    sock = socket(1, SOCK_STREAM, 0); /* { dg-message "when 'socket' fails" } */
    g_assert_cmpint(sock, !=, -1); /* this isn't marked "noreturn" */

    addr.sun_family = 1;
    snprintf(addr.sun_path, sizeof(addr.sun_path), "%s", socket_path);

    do {
        ret = bind(sock, (struct sockaddr *)&addr, sizeof(addr));
    } while (ret == -1 && errno == EINTR);
    g_assert_cmpint(ret, !=, -1);
    ret = listen(sock, 1); /* { dg-warning "'listen' on possibly invalid file descriptor 'sock'" } */
    g_assert_cmpint(ret, !=, -1);

    return sock;
}
