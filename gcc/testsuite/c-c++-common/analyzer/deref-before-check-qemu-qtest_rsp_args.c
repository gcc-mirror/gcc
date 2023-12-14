/* Reduced from qemu-7.2.0's tests/qtest/libqtest.c.  */

#define TRUE 1
#define NULL ((void *)0)

#define g_assert(expr) \
  do {									\
    if (expr) ; else /* { dg-warning "check of '\\*words' for NULL after already dereferencing it" } */ \
      g_assertion_message_expr (#expr);			\
} while (0)

void    g_assertion_message_expr        (const char     *expr) __attribute__((noreturn));

extern int strcmp (const char *__s1, const char *__s2)
  __attribute__ ((__nothrow__ , __leaf__, __pure__, __nonnull__ (1, 2)));
typedef char gchar;
typedef int gint;
typedef gint gboolean;
typedef struct _GString GString;

struct _GString
{
  gchar *str;
  /* [...snip...] */
};

extern
gchar* g_string_free (GString *string,
		      gboolean free_segment);
extern
gchar** g_strsplit (const gchar *string,
		    const gchar *delimiter,
		    gint max_tokens);
extern
void g_strfreev (gchar **str_array);

typedef struct QTestState QTestState;
typedef GString* (*QTestRecvFn)(QTestState *);

typedef struct QTestClientTransportOps {
    /* [...snip...] */
    QTestRecvFn recv_line;
} QTestTransportOps;

struct QTestState
{
    /* [...snip...] */
    QTestTransportOps ops;
    /* [...snip...] */
};

gchar **qtest_rsp_args(QTestState *s, int expected_args)
{
    GString *line;
    gchar **words;
    /* [...snip...] */

redo:
    line = s->ops.recv_line(s);
    words = g_strsplit(line->str, " ", 0);
    g_string_free(line, TRUE);

    if (strcmp(words[0], "IRQ") == 0) { /* { dg-message "pointer '\\*words' is dereferenced here" } */
        /* [...snip...] */
        g_strfreev(words);
        goto redo;
    }

    g_assert(words[0] != NULL); /* { dg-message "in expansion of macro 'g_assert'" } */
    /* [...snip...] */

    return words;
}
