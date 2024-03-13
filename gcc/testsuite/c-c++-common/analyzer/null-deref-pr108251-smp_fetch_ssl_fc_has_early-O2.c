/* Reduced from haproxy's src/ssl_sample.c  */

/* { dg-require-effective-target ptr_eq_long } */
/* { dg-additional-options "-O2 -Wno-analyzer-symbol-too-complex" } */

union sample_value {
  long long int sint;
  /* [...snip...]  */
};

struct sample_data {
 int type;
 union sample_value u;
};

enum {
  /* [...snip...]  */
 SMP_T_BOOL,
  /* [...snip...]  */
};
struct sample {
 unsigned int flags;
 struct sample_data data;
  /* [...snip...]  */
 struct session *sess;
 struct stream *strm;
  /* [...snip...]  */
};
struct arg {
  /* [...snip...]  */
};
enum obj_type {
 OBJ_TYPE_NONE = 0,
  /* [...snip...]  */
 OBJ_TYPE_CONN,
  /* [...snip...]  */
 OBJ_TYPE_CHECK,
 OBJ_TYPE_ENTRIES
};
enum {
  /* [...snip...]  */
 CO_FL_EARLY_SSL_HS = 0x00004000,
 CO_FL_EARLY_DATA = 0x00008000,
  /* [...snip...]  */
 CO_FL_SSL_WAIT_HS = 0x08000000,
  /* [...snip...]  */
};
struct connection {
 enum obj_type obj_type;
 unsigned char err_code;
  /* [...snip...]  */
 unsigned int flags;
  /* [...snip...]  */
};

static inline enum obj_type obj_type(const enum obj_type *t)
{
 if (!t || *t >= OBJ_TYPE_ENTRIES)
  return OBJ_TYPE_NONE;
 return *t;
}
static inline struct connection *__objt_conn(enum obj_type *t)
{
 return ((struct connection *)(((char *)(t)) - ((long)&((struct connection *)0)->obj_type))); /* { dg-bogus "may result in an unaligned pointer value" "Fixed in r14-6517-gb7e4a4c626e" { target short_enums } } */
}
static inline struct connection *objt_conn(enum obj_type *t)
{
 if (!t || *t != OBJ_TYPE_CONN)
   return (struct connection *)((void *)0);
 return __objt_conn(t);
}
struct session {
  /* [...snip...]  */
 enum obj_type *origin;
  /* [...snip...]  */
};
typedef struct ssl_st SSL;
SSL *ssl_sock_get_ssl_object(struct connection *conn);

/*****************************************************************************/

int
smp_fetch_ssl_fc_has_early(const struct arg *args, struct sample *smp, const char *kw, void *Private)
{
 SSL *ssl;
 struct connection *conn;

 conn = objt_conn(smp->sess->origin);
 ssl = ssl_sock_get_ssl_object(conn);
 if (!ssl)
  return 0;

 smp->flags = 0;
 smp->data.type = SMP_T_BOOL;
 smp->data.u.sint = ((conn->flags & CO_FL_EARLY_DATA) && /* { dg-bogus "dereference of NULL" "PR analyzer/108251" { xfail *-*-*} } */
     (conn->flags & (CO_FL_EARLY_SSL_HS | CO_FL_SSL_WAIT_HS))) ? 1 : 0;

 return 1;
}
