/* Reduced from qemu-7.2.0's qobject/json-parser.c, which
   is licensed under LGPLv2.1 or later.  */

/* { dg-additional-options "-fno-analyzer-call-summaries -Wno-analyzer-too-complex" } */

#define NULL ((void *)0)
typedef __builtin_va_list va_list;
typedef __SIZE_TYPE__ size_t;

typedef struct _GString GString;
typedef struct _GQueue GQueue;
typedef struct Error Error;
typedef struct QDict QDict;
typedef struct QList QList;
typedef struct QObject QObject;
typedef struct QString QString;

typedef enum QType {
  /* [...snip...] */
  QTYPE_QSTRING,
  /* [...snip...] */
} QType;

struct QObjectBase_ {
  QType type;
};

struct QObject {
  struct QObjectBase_ base;
};

#define QOBJECT(obj) ((QObject *)obj)
#define qobject_unref(OBJ) /* [...snip...] */

void qobject_ref_impl(QObject *obj);
QType qobject_type(const QObject *obj);
QObject *qobject_check_type(const QObject *obj, QType type);

typedef struct QTailQLink {
  void *tql_next;
  struct QTailQLink *tql_prev;
} QTailQLink;

typedef struct QDictEntry {
  char *key;
  QObject *value;
  struct {
    struct QDictEntry *le_next;
    struct QDictEntry **le_prev;
  } next;
} QDictEntry;

struct QDict {
  struct QObjectBase_ base;
  size_t size;
  struct {
    struct QDictEntry *lh_first;
  } table[512];
};

QDict *qdict_new(void);
void qdict_put_obj(QDict *qdict, const char *key, QObject *value);
int qdict_haskey(const QDict *qdict, const char *key);
typedef struct QListEntry {
  QObject *value;
  union {
    struct QListEntry *tqe_next;
    QTailQLink tqe_circ;
  } next;
} QListEntry;

struct QList {
  struct QObjectBase_ base;
  union {
    struct QListEntry *tqh_first;
    QTailQLink tqh_circ;
  } head;
};
QList *qlist_new(void);
void qlist_append_obj(QList *qlist, QObject *obj);

struct QString {
  struct QObjectBase_ base;
  const char *string;
};
QString *qstring_from_str(const char *str);
const char *qstring_get_str(const QString *qstring);

typedef enum json_token_type {
  JSON_ERROR = 0,

  JSON_LCURLY = 100,
  JSON_MIN = JSON_LCURLY,
  JSON_RCURLY,
  JSON_LSQUARE,
  JSON_RSQUARE,
  JSON_COLON,
  JSON_COMMA,
  JSON_INTEGER,
  JSON_FLOAT,
  JSON_KEYWORD,
  JSON_STRING,
  JSON_INTERP,
  JSON_END_OF_INPUT,
  JSON_MAX = JSON_END_OF_INPUT
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

void __attribute__((__format__(gnu_printf, 3, 4)))
parse_error(JSONParserContext *ctxt, JSONToken *token, const char *msg, ...);

JSONToken *parser_context_pop_token(JSONParserContext *ctxt);
JSONToken *parser_context_peek_token(JSONParserContext *ctxt);

static int parse_pair(JSONParserContext *ctxt, QDict *dict) {
  QObject *key_obj = NULL;
  QString *key;
  QObject *value;
  JSONToken *peek, *token;

  peek = parser_context_peek_token(ctxt);
  if (peek == NULL) {
    parse_error(ctxt, NULL, "premature EOI");
    goto out;
  }

  key_obj = parse_value(ctxt); /* { dg-bogus "infinite recursion" } */
  key = ((QString *)qobject_check_type(key_obj, QTYPE_QSTRING));
  if (!key) {
    parse_error(ctxt, peek, "key is not a string in object");
    goto out;
  }

  token = parser_context_pop_token(ctxt);
  if (token == NULL) {
    parse_error(ctxt, NULL, "premature EOI");
    goto out;
  }

  if (token->type != JSON_COLON) {
    parse_error(ctxt, token, "missing : in object pair");
    goto out;
  }

  value = parse_value(ctxt);
  if (value == NULL) {
    parse_error(ctxt, token, "Missing value in dict");
    goto out;
  }

  if (qdict_haskey(dict, qstring_get_str(key))) {
    parse_error(ctxt, token, "duplicate key");
    goto out;
  }

  qdict_put_obj(dict, qstring_get_str(key), value);

  qobject_unref(key_obj);
  return 0;

out:
  qobject_unref(key_obj);
  return -1;
}

static QObject *parse_object(JSONParserContext *ctxt) {
  QDict *dict = NULL;
  JSONToken *token, *peek;

  token = parser_context_pop_token(ctxt);

  dict = qdict_new();

  peek = parser_context_peek_token(ctxt);
  if (peek == NULL) {
    parse_error(ctxt, NULL, "premature EOI");
    goto out;
  }

  if (peek->type != JSON_RCURLY) {
    if (parse_pair(ctxt, dict) == -1) {
      goto out;
    }

    token = parser_context_pop_token(ctxt);
    if (token == NULL) {
      parse_error(ctxt, NULL, "premature EOI");
      goto out;
    }

    while (token->type != JSON_RCURLY) {
      if (token->type != JSON_COMMA) {
        parse_error(ctxt, token, "expected separator in dict");
        goto out;
      }

      if (parse_pair(ctxt, dict) == -1) {
        goto out;
      }

      token = parser_context_pop_token(ctxt);
      if (token == NULL) {
        parse_error(ctxt, NULL, "premature EOI");
        goto out;
      }
    }
  } else {
    (void)parser_context_pop_token(ctxt);
  }

  return QOBJECT(dict);

out:
  qobject_unref (dict);
  return NULL;
}

static QObject *parse_array(JSONParserContext *ctxt) {
  QList *list = NULL;
  JSONToken *token, *peek;

  token = parser_context_pop_token(ctxt);

  list = qlist_new();

  peek = parser_context_peek_token(ctxt);
  if (peek == NULL) {
    parse_error(ctxt, NULL, "premature EOI");
    goto out;
  }

  if (peek->type != JSON_RSQUARE) {
    QObject *obj;

    obj = parse_value(ctxt); /* { dg-bogus "infinite recursion" } */
    if (obj == NULL) {
      parse_error(ctxt, token, "expecting value");
      goto out;
    }

    qlist_append_obj(list, obj);

    token = parser_context_pop_token(ctxt);
    if (token == NULL) {
      parse_error(ctxt, NULL, "premature EOI");
      goto out;
    }

    while (token->type != JSON_RSQUARE) {
      if (token->type != JSON_COMMA) {
        parse_error(ctxt, token, "expected separator in list");
        goto out;
      }

      obj = parse_value(ctxt);
      if (obj == NULL) {
        parse_error(ctxt, token, "expecting value");
        goto out;
      }

      qlist_append_obj(list, obj);

      token = parser_context_pop_token(ctxt);
      if (token == NULL) {
        parse_error(ctxt, NULL, "premature EOI");
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
QObject *parse_literal(JSONParserContext *ctxt);

QObject *parse_value(JSONParserContext *ctxt) {
  JSONToken *token;

  token = parser_context_peek_token(ctxt);
  if (token == NULL) {
    parse_error(ctxt, NULL, "premature EOI");
    return NULL;
  }

  switch (token->type) {
  case JSON_LCURLY:
    return parse_object(ctxt); /* { dg-bogus "infinite recursion" } */
  case JSON_LSQUARE:
    return parse_array(ctxt); /* { dg-bogus "infinite recursion" } */
  case JSON_INTEGER:
  case JSON_FLOAT:
  case JSON_STRING:
    return parse_literal(ctxt);
  case JSON_KEYWORD:
    return parse_keyword(ctxt);
  default:
    parse_error(ctxt, token, "expecting value");
    return NULL;
  }
}
