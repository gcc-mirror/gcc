/* Test warnings for for GCC diagnostics.
   { dg-do compile }
   { dg-options "-Wformat -Wformat-diag" } */

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

/* Basic formatting function like pp_format.  */
void diag (const char*, ...) FORMAT (diag);

/* Diagnostic formatting function like error or warning declared
   by the C front end.  */
void cdiag (const char*, ...) FORMAT (cdiag);

/* Diagnostic formatting function like error or warning declared
   by the middle-end or back-end.  */
void tdiag (const char*, ...) FORMAT (tdiag);

/* Diagnostic formatting function like error or warning declared
   by the C++ front-end.  */
void cxxdiag (const char*, ...) FORMAT (cxxdiag);

void dump (const char*, ...) FORMAT (dump_printf);

/* Verify that functions declared with the C/C++ front-end __gcc_cdiag__
   attribute detect invalid whitespace in format strings.  */

void test_cdiag_whitespace (tree t, gimple *gc)
{
  (void)&t; (void)&gc;

  cdiag (" a");             /* { dg-warning "spurious leading space character in format" } */
  cdiag ("%< %>a");
  cdiag ("%<  %>a");
  cdiag ("a b");
  cdiag ("a  b");           /* { dg-warning "unquoted sequence of 2 consecutive space characters" } */
  cdiag ("a ");             /* { dg-warning "spurious trailing space character" } */
  cdiag ("a  ");            /* { dg-warning "spurious trailing space characters" } */
  cdiag ("a%< %>");
  cdiag ("a%< %>%< %>");
  cdiag ("a%< %> ");        /* { dg-warning "spurious trailing space character" } */
  cdiag ("a%< %>  %< %>");  /* { dg-warning "unquoted sequence of 2 consecutive space characters" } */

  /* It's debatable whether the following two formst strings should
     be  cdiagnosed.  They aren't only because it's simpler that way.  */
  cdiag ("a %< %>");
  cdiag ("a%< %> %< %>");

  /* Exercise other whitespace characters.  */
  cdiag ("a\fb");           /* { dg-warning "unquoted whitespace character '\\\\x0c'" } */
  cdiag ("a\nb");           /* { dg-warning "unquoted whitespace character '\\\\x0a'" } */
  cdiag ("a\rb");           /* { dg-warning "unquoted whitespace character '\\\\x0d'" } */
  cdiag ("a\vb");           /* { dg-warning "unquoted whitespace character '\\\\x0b'" } */

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-diag"

  /* Verify that the warning can be suppressed.  */
  cdiag ("\ta\b    c\vb\n");

#pragma GCC diagnostic pop
}


void test_cdiag_control (tree t, gimple *gc)
{
  (void)&t; (void)&gc;

  cdiag ("\1");             /* { dg-warning "unquoted control character '\\\\x01'" } */
  cdiag ("a\ab");           /* { dg-warning "unquoted control character '\\\\x07'" } */
  cdiag ("a\bb");           /* { dg-warning "unquoted control character '\\\\x08'" } */
}


void test_cdiag_punct (tree t, gimple *gc, int i)
{
  (void)&t; (void)&gc;

  /* Exercise the period.  */
  cdiag (".abc");           /* { dg-warning "spurious leading punctuation character" } */
  cdiag ("abc;");           /* { dg-warning "spurious trailing punctuation character" } */
  /* Verify that sentences that start with an uppercase letter and end
     in a period are not diagnosed.  */
  cdiag ("This is a full sentence.");
  cdiag ("Capitalized sentence (with a parethetical note).");
  cdiag ("Not a full sentence;");   /* { dg-warning "spurious trailing punctuation character" } */
  cdiag ("Neither is this one,");   /* { dg-warning "spurious trailing punctuation character" } */

  /* Exercise the ellipsis.  */
  cdiag ("this message...");
  cdiag ("...continues here");
  cdiag ("but...not here"); /* { dg-warning "unquoted sequence of 3 consecutive punctuation characters" } */

  /* Exercise parentheses.  */
  cdiag ("null argument where non-null required (argument %i)", i);

  /* Exercise C++.  */
  cdiag ("C++ is cool");
  cdiag ("this is c++");
  cdiag ("you can do this in C++ but not in C");

  /* Exercise parenthetical note followed by a colon, semicolon,
     or a comma.  */
  cdiag ("found a bug (here):");
  cdiag ("because of another bug (over there); fix it");

  cdiag ("found foo (123): go look at it");
  cdiag ("missed bar (abc); will try harder next time");

  cdiag ("expected this (or that), got something else (or who knows what)");
}


void test_cdiag_punct_balance (tree t, gimple *gc)
{
  (void)&t; (void)&gc;

  cdiag ("f()");            /* { dg-warning "spurious trailing punctuation characters" } */
  cdiag ("g(1)");
  cdiag ("(");              /* { dg-warning "spurious leading punctuation character|unbalanced" } */
  cdiag ("()");             /* { dg-warning "spurious leading punctuation characters" } */
  cdiag (")");              /* { dg-warning "unbalanced punctuation character '\\\)'" } */
  cdiag ("f()g");           /* { dg-warning "unquoted sequence of 2 consecutive punctuation characters" } */
  cdiag ("illegal operand (1)");
}


void test_cdiag_nongraph (tree t, gimple *gc)
{
  (void)&t; (void)&gc;

  cdiag ("a\376b");         /* { dg-warning "unquoted non-graph character '\\\\xfe'" } */
  cdiag ("a\377b");         /* { dg-warning "unquoted non-graph character '\\\\xff'" } */
}


void test_cdiag_attribute (tree t, gimple *gc)
{
  (void)&t; (void)&gc;

  cdiag ("attribute foo");
  cdiag ("this is attribute bar");
  cdiag ("bad __attribute bar");        /* { dg-warning "unquoted attribute" } */
  cdiag ("__attribute__ (foobar) bad"); /* { dg-warning "unquoted attribute" } */
  cdiag ("__attribute__ ((foobar))");   /* { dg-warning "unquoted attribute" } */
  cdiag ("__attribute__ (xxx))");       /* { dg-warning "unquoted attribute" } */
  /* { dg-warning "unbalanced punctuation character '\\\)'" "xxx" { target *-*-* } .-1 } */
  cdiag ("__attribute__ ((yyy)))");     /* { dg-warning "unquoted attribute" } */
  /* { dg-warning "unbalanced punctuation character '\\\)'" "yyy" { target *-*-* } .-1 } */
  cdiag ("__attribute__ ((zzz)");       /* { dg-warning "unquoted attribute" } */
  /* { dg-warning "unbalanced punctuation character '\\\('" "zzz" { target *-*-* } .-1 } */

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-diag"

  /* Verify that the warning can be suppressed.  */
  cdiag ("__attribute__ (((");

#pragma GCC diagnostic pop
}

void test_cdiag_builtin (tree t, gimple *gc)
{
  (void)&t; (void)&gc;

  cdiag ("__builtin_abort");    /* { dg-warning "unquoted name of built-in function '__builtin_abort'" } */
  cdiag ("in __builtin_trap");  /* { dg-warning "unquoted name of built-in function '__builtin_trap'" } */
  cdiag ("__builtin_xyz bites");/* { dg-warning "unquoted name of built-in function '__builtin_xyz'" } */

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-diag"

  /* Verify that the warning can be suppressed.  */
  cdiag ("__builtin____with____lots__of__underscores");

#pragma GCC diagnostic pop
}

void test_cdiag_option (tree t, gimple *gc)
{
  (void)&t; (void)&gc;

  cdiag ("%<-Wall%>");
  cdiag ("use option %<-Wextra%> to enable additinal warnings");

  cdiag ("-O2 is fast");       /* { dg-warning "unquoted option name '-O2'" } */
  cdiag ("but -O3 is faster"); /* { dg-warning "unquoted option name '-O3'" } */

  cdiag ("get --help");        /* { dg-warning "unquoted option name '--help'" } */
  cdiag ("enable -m32");       /* { dg-warning "unquoted option name '-m32'" } */
  cdiag ("value is -12");
  cdiag ("foo-O2");
  cdiag ("a-W");
}

void test_cdiag_oper (tree t, gimple *gc)
{
  cdiag ("a == b");            /* { dg-warning "unquoted operator '=='" } */
  cdiag ("++a");               /* { dg-warning "unquoted operator '\\\+\\\+'" } */
  cdiag ("b--");               /* { dg-warning "unquoted operator '--'" } */
}
