/* Reduced from qemu-7.2.0's qobject/json-parser.c, which
   is licensed under LGPLv2.1 or later.  */

/* { dg-additional-options "-fno-analyzer-call-summaries -Wno-analyzer-too-complex" } */

#include "../../gcc.dg/analyzer/analyzer-decls.h"


typedef __builtin_va_list va_list;

typedef struct _GQueue GQueue;
typedef struct Error Error;
typedef struct QList QList;
typedef struct QObject QObject;

struct QObjectBase_ {
  /* [...snip...] */
};


struct QObject {
  struct QObjectBase_ base;
};

#define QOBJECT(obj) ((QObject *)obj)
#define qobject_unref(OBJ) /* [...snip...] */

typedef struct QTailQLink {
  void *tql_next;
  struct QTailQLink *tql_prev;
} QTailQLink;

struct QList {
  struct QObjectBase_ base;
  union {
    struct QListEntry *tqh_first;
    QTailQLink tqh_circ;
  } head;
};
QList *qlist_new(void);
void qlist_append_obj(QList *qlist, QObject *obj);

typedef enum json_token_type {
  /* [...snip...] */			      
  JSON_LSQUARE,
  JSON_RSQUARE,
  /* [...snip...] */			      
  JSON_COMMA,
  /* [...snip...] */			      
  JSON_KEYWORD,
  /* [...snip...] */			      
} JSONTokenType;
typedef struct JSONToken JSONToken;

struct JSONToken {
  JSONTokenType type;
  int x;
  int y;
  char str[];
};

typedef struct JSONParserContext {
  Error *err;
  JSONToken *current;
  GQueue *buf;
  va_list *ap;
} JSONParserContext;
static QObject *parse_value(JSONParserContext *ctxt);

JSONToken *parser_context_pop_token(JSONParserContext *ctxt);
JSONToken *parser_context_peek_token(JSONParserContext *ctxt);

static QObject *parse_array(JSONParserContext *ctxt) {
  QList *list = NULL;
  JSONToken *token, *peek;

  token = parser_context_pop_token(ctxt);

  list = qlist_new();

  peek = parser_context_peek_token(ctxt);
  if (peek == NULL) {
    goto out;
  }

  if (peek->type != JSON_RSQUARE) {
    QObject *obj;

    obj = parse_value(ctxt); /* { dg-bogus "infinite recursion" } */
    if (obj == NULL) {
      goto out;
    }

    qlist_append_obj(list, obj);

    token = parser_context_pop_token(ctxt);
    if (token == NULL) {
      goto out;
    }

    while (token->type != JSON_RSQUARE) {
      if (token->type != JSON_COMMA) {
        goto out;
      }

      obj = parse_value(ctxt);
      if (obj == NULL) {
        goto out;
      }

      qlist_append_obj(list, obj);

      token = parser_context_pop_token(ctxt);
      if (token == NULL) {
        goto out;
      }
    }
  } else {
    (void)parser_context_pop_token(ctxt);
  }

  return QOBJECT(list);

out:
  qobject_unref(list);
  return NULL;
}

QObject *parse_keyword(JSONParserContext *ctxt);

QObject *parse_value(JSONParserContext *ctxt) {
  JSONToken *token;

  token = parser_context_peek_token(ctxt);
  if (token == NULL) {
    return NULL;
  }

  switch (token->type) {
  case JSON_LSQUARE:
    return parse_array(ctxt); /* { dg-bogus "infinite recursion" } */
  case JSON_KEYWORD:
    return parse_keyword(ctxt);
  default:
    return NULL;
  }
}
