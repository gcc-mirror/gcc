/* Test for GCC internal format directives.
   { dg-do compile }
   { dg-options "-std=gnu99 -Wformat" } */

/* Magic identifiers must be set before the attribute is used.  */

typedef long long __gcc_host_wide_int__;

typedef struct location_s
{
  const char *file;
  int line;
} location_t;

union tree_node;
typedef union tree_node *tree;

/* Define gimple as a dummy type.  The typedef must be provided for
   the C test to find the symbol.  */
typedef struct gimple gimple;

/* Likewise for gimple.  */
typedef struct cgraph_node cgraph_node;

#define FORMAT(kind) __attribute__ ((format (__gcc_## kind ##__, 1, 2)))

void diag (const char*, ...) FORMAT (diag);
void cdiag (const char*, ...) FORMAT (cdiag);
void tdiag (const char*, ...) FORMAT (tdiag);
void cxxdiag (const char*, ...) FORMAT (cxxdiag);
void dump (const char*, ...) FORMAT (dump_printf);

void test_diag (tree t, gimple *gc)
{
  diag ("%<");   /* { dg-warning "unterminated quoting directive" } */
  diag ("%>");   /* { dg-warning "unmatched quoting directive " } */
  diag ("%<foo%<bar%>%>");   /* { dg-warning "nested quoting directive" } */

  diag ("%G", gc); /* { dg-warning "format" } */
  diag ("%K", t); /* { dg-warning "format" } */

  diag ("%R");       /* { dg-warning "unmatched color reset directive" } */
  diag ("%r", "");   /* { dg-warning "unterminated color directive" } */
  diag ("%r%r", "", "");   /* { dg-warning "unterminated color directive" } */
  diag ("%r%R", "");
  diag ("%r%r%R", "", "");
  diag ("%r%R%r%R", "", "");

  diag ("%<%R%>");      /* { dg-warning "unmatched color reset directive" } */
  diag ("%<%r%>", "");  /* { dg-warning "unterminated color directive" } */
  diag ("%<%r%R%>", "");
}

void test_cdiag (tree t, gimple *gc)
{
  cdiag ("%<");   /* { dg-warning "unterminated quoting directive" } */
  cdiag ("%>");   /* { dg-warning "unmatched quoting directive " } */
  cdiag ("%<foo%<bar%>%>");   /* { dg-warning "nested quoting directive" } */

  cdiag ("%D", t);       /* { dg-warning ".D. conversion used unquoted" } */
  cdiag ("%E", t);
  cdiag ("%F", t);       /* { dg-warning ".F. conversion used unquoted" } */
  cdiag ("%G", gc);
  cdiag ("%K", t);

  cdiag ("%R");       /* { dg-warning "unmatched color reset directive" } */
  cdiag ("%r", "");   /* { dg-warning "unterminated color directive" } */
  cdiag ("%r%r", "", "");   /* { dg-warning "unterminated color directive" } */
  cdiag ("%r%R", "");
  cdiag ("%r%r%R", "", "");
  cdiag ("%r%R%r%R", "", "");

  cdiag ("%T", t);       /* { dg-warning ".T. conversion used unquoted" } */
  cdiag ("%V", t);       /* { dg-warning ".V. conversion used unquoted" } */

  cdiag ("%<%D%>", t);
  cdiag ("%<%E%>", t);
  cdiag ("%<%F%>", t);
  cdiag ("%<%G%>", gc);  /* { dg-warning ".G. conversion used within a quoted sequence" } */
  cdiag ("%<%K%>", t);   /* { dg-warning ".K. conversion used within a quoted sequence" } */

  cdiag ("%<%R%>");      /* { dg-warning "unmatched color reset directive" } */
  cdiag ("%<%r%>", "");  /* { dg-warning "unterminated color directive" } */
  cdiag ("%<%r%R%>", "");

  cdiag ("%<%T%>", t);
  cdiag ("%<%V%>", t);

  cdiag ("%<%qD%>", t);  /* { dg-warning ".q. flag used within a quoted sequence" } */
  cdiag ("%<%qE%>", t);  /* { dg-warning ".q. flag used within a quoted sequence" } */
  cdiag ("%<%qT%>", t);  /* { dg-warning ".q. flag used within a quoted sequence" } */
}

void test_tdiag (tree t, gimple *gc)
{
  tdiag ("%<");       /* { dg-warning "unterminated quoting directive" } */
  tdiag ("%>");       /* { dg-warning "unmatched quoting directive " } */
  tdiag ("%<foo%<bar%>%>");   /* { dg-warning "nested quoting directive" } */

  tdiag ("%D", t);       /* { dg-warning ".D. conversion used unquoted" } */
  tdiag ("%E", t);
  tdiag ("%G", gc);
  tdiag ("%K", t);

  tdiag ("%R");          /* { dg-warning "unmatched color reset directive" } */
  tdiag ("%r", "");   /* { dg-warning "unterminated color directive" } */
  tdiag ("%r%r", "", "");   /* { dg-warning "unterminated color directive" } */
  tdiag ("%r%R", "");
  tdiag ("%r%R", "");
  tdiag ("%r%r%R", "", "");
  tdiag ("%r%R%r%R", "", "");

  tdiag ("%T", t);       /* { dg-warning ".T. conversion used unquoted" } */

  tdiag ("%<%D%>", t);
  tdiag ("%<%E%>", t);
  tdiag ("%<%G%>", gc);  /* { dg-warning ".G. conversion used within a quoted sequence" } */
  tdiag ("%<%K%>", t);   /* { dg-warning ".K. conversion used within a quoted sequence" } */

  tdiag ("%<%R%>");      /* { dg-warning "unmatched color reset directive" } */
  tdiag ("%<%r%>", "");  /* { dg-warning "unterminated color directive" } */
  tdiag ("%<%r%R%>", "");

  tdiag ("%<%T%>", t);

  tdiag ("%<%qD%>", t);  /* { dg-warning ".q. flag used within a quoted sequence" } */
  tdiag ("%<%qE%>", t);  /* { dg-warning ".q. flag used within a quoted sequence" } */
  tdiag ("%<%qT%>", t);  /* { dg-warning ".q. flag used within a quoted sequence" } */
}

void test_cxxdiag (tree t, gimple *gc)
{
  cxxdiag ("%A", t);     /* { dg-warning ".A. conversion used unquoted" } */
  cxxdiag ("%D", t);     /* { dg-warning ".D. conversion used unquoted" } */
  cxxdiag ("%E", t);
  cxxdiag ("%F", t);     /* { dg-warning ".F. conversion used unquoted" } */
  cxxdiag ("%G", gc);
  cxxdiag ("%K", t);

  cxxdiag ("%R");        /* { dg-warning "unmatched color reset directive" } */
  cxxdiag ("%r", "");    /* { dg-warning "unterminated color directive" } */
  cxxdiag ("%r%r", "", "");   /* { dg-warning "unterminated color directive" } */
  cxxdiag ("%r%R", "");
  cxxdiag ("%r%R", "");
  cxxdiag ("%r%r%R", "", "");
  cxxdiag ("%r%R%r%R", "", "");

  cxxdiag ("%S", t);     /* { dg-warning ".S. conversion used unquoted" } */
  cxxdiag ("%T", t);     /* { dg-warning ".T. conversion used unquoted" } */
  cxxdiag ("%V", t);     /* { dg-warning ".V. conversion used unquoted" } */
  cxxdiag ("%X", t);     /* { dg-warning ".X. conversion used unquoted" } */

  cxxdiag ("%<%A%>", t);
  cxxdiag ("%<%D%>", t);
  cxxdiag ("%<%E%>", t);
  cxxdiag ("%<%F%>", t);
  cxxdiag ("%<%R%>");    /* { dg-warning "unmatched color reset" } */
  cxxdiag ("%<%r%R%>", "");
  cxxdiag ("%<%S%>", t);
  cxxdiag ("%<%T%>", t);
  cxxdiag ("%<%V%>", t);
  cxxdiag ("%<%X%>", t);
}

void test_dump (tree t, gimple *stmt, cgraph_node *node)
{
  dump ("%<");   /* { dg-warning "unterminated quoting directive" } */
  dump ("%>");   /* { dg-warning "unmatched quoting directive " } */
  dump ("%<foo%<bar%>%>");   /* { dg-warning "nested quoting directive" } */

  dump ("%R");       /* { dg-warning "unmatched color reset directive" } */
  dump ("%r", "");   /* { dg-warning "unterminated color directive" } */
  dump ("%r%r", "", "");   /* { dg-warning "unterminated color directive" } */
  dump ("%r%R", "");
  dump ("%r%r%R", "", "");
  dump ("%r%R%r%R", "", "");

  dump ("%<%R%>");      /* { dg-warning "unmatched color reset directive" } */
  dump ("%<%r%>", "");  /* { dg-warning "unterminated color directive" } */
  dump ("%<%r%R%>", "");

  dump ("%E", stmt);
  dump ("%T", t);
  dump ("%G", stmt);
  dump ("%C", node);
  dump ("%f", 1.0);
  dump ("%4.2f", 1.0); /* { dg-warning "format" } */
}
