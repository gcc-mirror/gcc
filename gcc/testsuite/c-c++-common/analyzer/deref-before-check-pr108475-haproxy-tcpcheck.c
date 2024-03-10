/* Reduced from haproxy-2.7.1: src/tcpcheck.c.  */

/* { dg-additional-options "-Wno-analyzer-too-complex" } */

typedef __SIZE_TYPE__ size_t;

#include "../../gcc.dg/analyzer/analyzer-decls.h"


extern void *calloc(size_t __nmemb, size_t __size)
    __attribute__((__nothrow__, __leaf__)) __attribute__((__malloc__))
    __attribute__((__alloc_size__(1, 2)));
extern char *strdup(const char *__s) __attribute__((__nothrow__, __leaf__))
__attribute__((__malloc__)) __attribute__((__nonnull__(1)));
extern char *strstr(const char *__haystack, const char *__needle)
    __attribute__((__nothrow__, __leaf__)) __attribute__((__pure__))
    __attribute__((__nonnull__(1, 2)));
extern size_t strlen(const char *__s) __attribute__((__nothrow__, __leaf__))
__attribute__((__pure__)) __attribute__((__nonnull__(1)));
struct list {
  struct list *n;
  struct list *p;
};
struct buffer {
  size_t size;
  char *area;
  size_t data;
  size_t head;
};
struct proxy;
struct ist {
  char *ptr;
  size_t len;
};
static inline int isttest(const struct ist ist) { return ist.ptr != NULL; }

enum http_meth_t {
  HTTP_METH_OPTIONS,
  /* [...snip...] */
} __attribute__((packed));

struct http_meth {
  enum http_meth_t meth;
  struct buffer str;
};
enum tcpcheck_send_type {
  /* [...snip...] */
  TCPCHK_SEND_HTTP,
};
enum tcpcheck_rule_type {
  TCPCHK_ACT_SEND = 0,
  /* [...snip...] */
};
struct tcpcheck_http_hdr {
  struct ist name;
  struct list value;
  struct list list;
};
struct tcpcheck_send {
  enum tcpcheck_send_type type;
  union {
    /* [...snip...] */
    struct {
      unsigned int flags;
      struct http_meth meth;
      union {
        struct ist uri;
        /* [...snip...] */
      };
      struct ist vsn;
      struct list hdrs;
      /* [...snip...] */
    } http;
  };
};
struct tcpcheck_rule {
  /* [...snip...] */
  enum tcpcheck_rule_type action;
  /* [...snip...] */
  union {
    /* [...snip...] */
    struct tcpcheck_send send;
    /* [...snip...] */
  };
};
enum http_meth_t find_http_meth(const char *str, const int len);
void free_tcpcheck(struct tcpcheck_rule *rule, int in_pool);
void free_tcpcheck_http_hdr(struct tcpcheck_http_hdr *hdr);

#define ist(str) ({                                                    \
	char *__x = (char *) ((void *)(str));                                     \
	(struct ist){                                                  \
		.ptr = __x,                                            \
		.len = __builtin_constant_p(str) ?                     \
			((void *)str == (void *)0) ? 0 :               \
			__builtin_strlen(__x) :                        \
			({                                             \
				size_t __l = 0;                        \
				if (__x) for (__l--; __x[++__l]; ) ;   \
				__l;                                   \
			})                                             \
	};                                                             \
})

struct tcpcheck_rule *proxy_parse_httpchk_req(char **args, int cur_arg,
                                              struct proxy *px, char **errmsg) {
  struct tcpcheck_rule *chk = NULL;
  struct tcpcheck_http_hdr *hdr = NULL;
  char *meth = NULL, *uri = NULL, *vsn = NULL;
  char *hdrs, *body;

  hdrs = (*args[cur_arg + 2] ? strstr(args[cur_arg + 2], "\r\n") : NULL);
  body = (*args[cur_arg + 2] ? strstr(args[cur_arg + 2], "\r\n\r\n") : NULL);
  if (hdrs || body) {
    /* [...snip...] */
    goto error;
  }

  chk = (struct tcpcheck_rule *) calloc(1, sizeof(*chk));
  if (!chk) {
    /* [...snip...] */
    goto error;
  }
  chk->action = TCPCHK_ACT_SEND;
  chk->send.type = TCPCHK_SEND_HTTP;
  chk->send.http.flags |= 0x0004;
  chk->send.http.meth.meth = HTTP_METH_OPTIONS;
  ((&chk->send.http.hdrs)->n = (&chk->send.http.hdrs)->p =
       (&chk->send.http.hdrs));

  if (*args[cur_arg]) {
    if (!*args[cur_arg + 1])
      uri = args[cur_arg];
    else
      meth = args[cur_arg];
  }
  if (*args[cur_arg + 1])
    uri = args[cur_arg + 1];
  if (*args[cur_arg + 2])
    vsn = args[cur_arg + 2];

  if (meth) { /* { dg-bogus "check of 'meth' for NULL after already dereferencing it" } */
    chk->send.http.meth.meth = find_http_meth(meth, strlen(meth));
    chk->send.http.meth.str.area = strdup(meth);
    chk->send.http.meth.str.data = strlen(meth);
    if (!chk->send.http.meth.str.area) {
      /* [...snip...] */
      goto error;
    }
  }
  if (uri) {
    chk->send.http.uri = ist(strdup(uri));
    if (!isttest(chk->send.http.uri)) {
      /* [...snip...] */
      goto error;
    }
  }
  if (vsn) { /* { dg-bogus "check of 'vsn' for NULL after already dereferencing it" } */
    chk->send.http.vsn = ist(strdup(vsn));
    if (!isttest(chk->send.http.vsn)) {
      /* [...snip...] */
      goto error;
    }
  }
  return chk;

error:
  free_tcpcheck_http_hdr(hdr);
  free_tcpcheck(chk, 0);
  return NULL;
}
