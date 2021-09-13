/* Test warnings for common punctuation, quoting, and spelling issues
   in GCC diagnostics.
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

/* Likewise for cgraph_node.  */
typedef struct cgraph_node cgraph_node;

#define ATTR(...)    __attribute__ ((__VA_ARGS__))
#define FORMAT(kind) ATTR (format (__gcc_## kind ##__, 1, 2))

/* Raw formatting function like pp_format.  */
void diag_raw (const char*, ...) ATTR (format (__gcc_diag_raw__, 1, 2));
void cdiag_raw (const char*, ...) ATTR (format (__gcc_cdiag_raw__, 1, 2));
void tdiag_raw (const char*, ...) ATTR (format (gcc_tdiag_raw, 1, 2));
void cxxdiag_raw (const char*, ...) ATTR (format (gcc_cxxdiag_raw, 1, 2));

/* Basic formatting function_format.  */
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


/* Verify that functions declared with __gcc_diag_raw__ attribute
   are not subject to -Wformat-diag.  */

void test_diag_raw (tree t, gimple *gc)
{
  diag_raw ("a  b");
  diag_raw ("newline\n");
  diag_raw ("lone period.");
  diag_raw ("multiple punctuators: !!!");
  diag_raw ("unbalanced paren (");
  diag_raw ("keyword alignas and identifier_with_underscores");
  diag_raw ("disable __builtin_abs with the -fno-builtin-abs option");
  diag_raw ("who says I can't have no stinkin' contractions? ");

  cdiag_raw ("__atomic_sync (%qE) == 7???", t);
  tdiag_raw ("__builtin_abs (%E) < 0!?!", t);
  cxxdiag_raw ("template <> int f (%E", t);
}

/* Verify that functions declared with the C front-end __gcc_cdiag__
   attribute detect invalid whitespace in format strings.  */

void test_cdiag_whitespace (tree t, gimple *gc)
{
  (void)&t; (void)&gc;

  /* Verify that strings of leading spaces don't trigger a warning.  */
  cdiag (" a");
  cdiag ("  b");
  cdiag ("   c");
  cdiag ("%< %>a");
  cdiag ("%<  %>a");
  cdiag ("a b");
  cdiag ("a  b");           /* { dg-warning "unquoted sequence of 2 consecutive space characters" } */
  cdiag ("a ");             /* { dg-warning "spurious trailing space" } */
  cdiag ("a  ");            /* { dg-warning "spurious trailing space" } */
  cdiag ("a%< %>");
  cdiag ("a%< %>%< %>");
  cdiag ("a%< %> ");        /* { dg-warning "spurious trailing space" } */
  cdiag ("a%< %>  %< %>");  /* { dg-warning "unquoted sequence of 2 consecutive space characters" } */

  /* It's debatable whether the following two formst strings should
     be diagnosed.  They aren't only because it's simpler that way.  */
  cdiag ("a %< %>");
  cdiag ("a%< %> %< %>");

  /* Exercise other whitespace characters.  */
  cdiag ("a\fb");           /* { dg-warning "unquoted whitespace character '\\\\x0c'" } */
  cdiag ("a\nb");           /* { dg-warning "unquoted whitespace character '\\\\x0a'" } */
  cdiag ("a\rb");           /* { dg-warning "unquoted whitespace character '\\\\x0d'" } */
  cdiag ("a\vb");           /* { dg-warning "unquoted whitespace character '\\\\x0b'" } */

  cdiag ("First sentence.  And a next.");
  cdiag ("First sentence.  not capitalized sentence"); /* { dg-warning "inconsistent capitalization" } */

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
  cdiag (".abc");           /* { dg-warning "spurious leading punctuation sequence .\.." } */
  cdiag ("abc;");           /* { dg-warning "spurious trailing punctuation sequence .;." } */
  /* Verify that sentences that start with an uppercase letter and end
     in a period are not diagnosed.  */
  cdiag ("This is a full sentence.");
  cdiag ("Capitalized sentence (with a parethetical note).");
  cdiag ("Not a full sentence;");   /* { dg-warning "spurious trailing punctuation sequence .;." } */
  cdiag ("Neither is this one,");   /* { dg-warning "spurious trailing punctuation sequence .,." } */

  /* Exercise the ellipsis.  */
  cdiag ("this message...");
  cdiag ("...continues here");
  cdiag ("but...not here"); /* { dg-warning "unquoted sequence of 3 consecutive punctuation characters" } */

  /* Verify that parenthesized sentences are accepted, even the whole
     meesage (done in the C++ front end).  */
  cdiag ("null argument where non-null required (argument %i)", i);
  cdiag ("null (argument %i) where non-null required", i);
  cdiag ("(see what comes next)");

  /* Verify that only a single trailing colon is accepted.  */
  cdiag ("candidates are:");
  cdiag ("candidates are::"); /* { dg-warning "spurious trailing punctuation sequence .::." } */

  /* Exercise C++.  */
  cdiag ("C++ is cool");
  cdiag ("this is c++");
  cdiag ("you can do this in C++ but not in C");

  /* Also verify that G++ is accepted.  */
  cdiag ("G++ rocks");
  cdiag ("this is accepted by g++");
  cdiag ("valid in G++ (or g++) but not in gcc");

  /* Exercise parenthetical note followed by a colon, semicolon,
     or a comma.  */
  cdiag ("found a bug (here):");
  cdiag ("because of another bug (over there); fix it");

  cdiag ("found foo (123): go look at it");
  cdiag ("missed bar (abc); will try harder next time");

  cdiag ("expected this (or that), got something else (or who knows what)");

  /* Exercise parenthetical note with a question mark.  */
  cdiag ("hmmm (did you really mean that?)");
  cdiag ("error (did you mean %<foo()%>?)");
  /* And a question mark after a parenthetical note.  */
  cdiag ("did you mean this (or that)?");

  /* But make sure unbalanced parenthese are diagnosed.  */
  cdiag ("or this or the other)?");   /* { dg-warning "unbalanced punctuation character '\\\)'" } */

  cdiag ("## Heading");               /* { dg-warning "spurious leading punctuation sequence .##." } */
  cdiag ("## %s ##", "1");            /* { dg-warning "spurious (leading|trailing) punctuation sequence .##." } */

  cdiag ("#1 priority");              /* { dg-warning "spurious leading punctuation sequence .#." } */
  cdiag ("priority #2");

  /* Quoting.  */
  cdiag ("\"quoted\"");
  cdiag ("\"quoted\" string");
  cdiag ("this is a \"string in quotes\"");
  cdiag ("\"missing closing quote");  /* { dg-warning "unterminated quote character '\"'" } */

  /* PR translation/90121 - punctuation character after a space.  */
  cdiag ("bad version : 1");          /* { dg-warning "space followed by punctuation character ':'" } */
  cdiag ("problem ; fix it");         /* { dg-warning "space followed by punctuation character ';'" } */
  cdiag ("End . not.");               /* { dg-warning "space followed by punctuation character '.'" } */
  cdiag ("it is bad , very bad");     /* { dg-warning "space followed by punctuation character ','" } */
  cdiag ("say what ?");               /* { dg-warning "space followed by punctuation character '?'" } */

  /* But these are okay after a space.  But should they be?  */
  cdiag ("1 / 2");
  cdiag ("2 + 3");
  cdiag ("2 - 3");
}

void test_cdiag_punct_balance (tree t, gimple *gc)
{
  (void)&t; (void)&gc;

  /* Less-than and greater than.  */
  cdiag ("a < b");          /* { dg-warning "unbalanced punctuation character '<' in format" } */
  cdiag ("must be > 0");    /* { dg-warning "unbalanced punctuation character '>' in format" } */

  cdiag ("f()");            /* { dg-warning "spurious trailing punctuation sequence .\\\(\\\)." } */
  cdiag ("g(1)");
  cdiag ("(");              /* { dg-warning "spurious leading punctuation character|unbalanced" } */
  cdiag ("()");             /* { dg-warning "spurious leading punctuation sequence" } */
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

  cdiag ("-O2 is fast");        /* { dg-warning "unquoted option name '-O2'" } */
  cdiag ("but -O3 is faster");  /* { dg-warning "unquoted option name '-O3'" } */

  cdiag ("get --help");         /* { dg-warning "unquoted option name '--help'" } */
  cdiag ("enable -m32");        /* { dg-warning "unquoted option name '-m32'" } */
  cdiag ("value is -12");
  cdiag ("foo-O2");
  cdiag ("a-W");
}


void test_cdiag_keyword (tree t, gimple *gc)
{
  cdiag ("alignasi");
  cdiag ("malignofer or alignofus");
  cdiag ("use alignof");        /* { dg-warning "unquoted keyword 'alignof'" } */
  cdiag ("or _Alignof");        /* { dg-warning " keyword '_Alignof'" } */
  cdiag ("_Pragma too");        /* { dg-warning " keyword '_Pragma'" } */

  cdiag ("a #error directive"); /* { dg-warning "unquoted preprocessing directive '#error'" } */
  cdiag ("#include file");      /* { dg-warning "unquoted preprocessing directive '#include'" } */
  cdiag ("but #pragma foobar"); /* { dg-warning "unquoted preprocessing directive '#pragma'" } */
  cdiag ("pragma foobar is okay");
  cdiag ("or even # pragma is fine");

  /* Exercise qualifiers.  */
  cdiag ("const function");
  cdiag ("const-qualified variable"); /* { dg-warning "unquoted keyword 'const-qualified'" } */
  /* { dg-message "use '%<const%>-qualified' instead" "const-qualified" { target *-*-* } .-1 } */
  cdiag ("a const %qD", t);     /* { dg-warning "unquoted keyword 'const'" } */
  cdiag ("restrict %qE", t);    /* { dg-warning "unquoted keyword 'restrict'" } */
  cdiag ("volatile %qT", t);    /* { dg-warning "unquoted keyword 'volatile'" } */
  cdiag ("const %qD and restrict %qE or volatile %qT", t, t, t);
  /* { dg-warning "unquoted keyword 'const'" "" { target *-*-* } .-1 } */
  /* { dg-warning "unquoted keyword 'restrict'" "" { target *-*-* } .-2 } */
  /* { dg-warning "unquoted keyword 'volatile'" "" { target *-*-* } .-3 } */

  cdiag ("an offsetof here");   /* { dg-warning "unquoted keyword 'offsetof" } */
  cdiag ("sizeof x");           /* { dg-warning "unquoted keyword 'sizeof" } */
  cdiag ("have typeof");        /* { dg-warning "unquoted keyword 'typeof" } */

  /* Words that are not keywords are so are not expected to be quoted.  */
  cdiag ("break rules");
  cdiag ("if we continue by default for a short while else do nothing");
  cdiag ("register a function for unsigned extern to void const reads");
  cdiag ("or volatile access");
}


void test_cdiag_operator (tree t, gimple *gc)
{
  cdiag ("x != 0");             /* { dg-warning "unquoted operator '!='" } */
  cdiag ("logical &&");         /* { dg-warning "unquoted operator '&&" } */
  cdiag ("+= operator");        /* { dg-warning "unquoted operator '\\\+=" } */
  cdiag ("a == b");             /* { dg-warning "unquoted operator '=='" } */
  cdiag ("++a");                /* { dg-warning "unquoted operator '\\\+\\\+'" } */
  cdiag ("b--");                /* { dg-warning "unquoted operator '--'" } */
  cdiag ("1 << 2");             /* { dg-warning "unquoted operator '<<'" } */
  cdiag (">> here <<");         /* { dg-warning "unquoted operator '>>|<<'" } */
}


void test_cdiag_type_name (tree t, gimple *gc)
{
  cdiag ("the word character should not be quoted");
  cdiag ("but char should be"); /* { dg-warning "unquoted keyword 'char'" } */

  cdiag ("unsigned char should be quoted");     /* { dg-warning "unquoted type name 'unsigned char'" } */
  cdiag ("but unsigned character is fine");

  cdiag ("as should int");      /* { dg-warning "unquoted keyword 'int'" } */
  cdiag ("and signed int");     /* { dg-warning "unquoted type name 'signed int'" } */
  cdiag ("and also unsigned int");     /* { dg-warning "unquoted type name 'unsigned int'" } */
  cdiag ("very long thing");
  cdiag ("use long long here"); /* { dg-warning "unquoted type name 'long long'" } */

  cdiag ("have a floating type");
  cdiag ("found float type");   /* { dg-warning "unquoted keyword 'float'" } */

  cdiag ("wchar_t is wide");    /* { dg-warning "unquoted identifier or keyword 'wchar_t'" } */
}


void test_cdiag_identifier (tree t, gimple *gc)
{
  (void)&t; (void)&gc;

  cdiag ("private _x ident");   /* { dg-warning "unquoted identifier or keyword '_x'" } */
  cdiag ("and another __y");    /* { dg-warning "unquoted identifier or keyword '__y'" } */
  cdiag ("ident z_ with trailing underscore");   /* { dg-warning "unquoted identifier or keyword 'z_'" } */
  cdiag ("v_ variable");        /* { dg-warning "unquoted identifier or keyword 'v_'" } */
  cdiag ("call foo_bar");       /* { dg-warning "unquoted identifier or keyword 'foo_bar'" } */
  cdiag ("unquoted x_y ident");  /* { dg-warning "unquoted identifier or keyword 'x_y'" } */

  cdiag ("size_t type");        /* { dg-warning "unquoted identifier or keyword 'size_t'" } */
  cdiag ("bigger than INT_MAX");/* { dg-warning "unquoted identifier or keyword 'INT_MAX'" } */

  cdiag ("quoted ident %<a_b%>");
  cdiag ("another quoted identifier %<x_%> here");
}


void test_cdiag_bad_words (tree t, gimple *gc)
{
  (void)&t; (void)&gc;

  cdiag ("aren't you dumb?");  /* { dg-warning "bare apostrophe ''' in format" } */
  cdiag ("bitfields suck");    /* { dg-warning "misspelled term 'bitfields' in format; use 'bit-fields' instead" } */
  cdiag ("invalid bitfield");  /* { dg-warning "misspelled term 'bitfield' in format; use 'bit-field' instead" } */
  cdiag ("bad builtin function");  /* { dg-warning "misspelled term 'builtin function' in format; use 'built-in function' instead" } */
  cdiag ("bad builtin function");  /* { dg-warning "misspelled term 'builtin function' in format; use 'built-in function' instead" } */
  cdiag ("builtin function x");    /* { dg-warning "misspelled term 'builtin function' in format; use 'built-in function' instead" } */
  cdiag ("builtin functions disabled");    /* { dg-warning "misspelled term 'builtin functions' in format; use 'built-in functions' instead" } */
  cdiag ("enable builtin functions");      /* { dg-warning "misspelled term 'builtin functions' in format; use 'built-in functions' instead" } */
  cdiag ("you can't do that"); /* { dg-warning "contraction 'can't' in format" } */
  cdiag ("you can%'t do that");/* { dg-warning "contraction 'can%'t' in format" } */
  cdiag ("Can%'t touch this.");/* { dg-warning "contraction 'Can%'t' in format" } */
  cdiag ("can%'");
  cdiag ("can%' whatever");
  cdiag ("on the commandline");/* { dg-warning "misspelled term 'commandline' in format; use 'command line' instead" } */
  cdiag ("command line option");/* { dg-warning "misspelled term 'command line option' in format; use 'command-line option' instead" } */
  cdiag ("it mustn't be");     /* { dg-warning "contraction 'mustn't' in format" } */
  cdiag ("isn't that silly?"); /* { dg-warning "bare apostrophe ''' in format" } */

  cdiag ("can not do this");   /* { dg-warning "misspelled term 'can not' in format; use 'cannot' instead" } */
  cdiag ("you can not");       /* { dg-warning "misspelled term 'can not' in format; use 'cannot' instead" } */

  /* See PR target/90157 - aarch64: unnecessary abbreviation in diagnostic */
  cdiag ("Mising arg.");       /* { dg-warning "misspelled term 'arg' in format; use 'argument' instead" } */
  cdiag ("2 args: a and b");   /* { dg-warning "misspelled term 'args' in format; use 'arguments' instead" } */
  cdiag ("arg 1");             /* { dg-warning "misspelled term 'arg' in format; use 'argument' instead" } */
  cdiag ("Args are wrong.");   /* { dg-warning "misspelled term 'Args' in format; use 'arguments' instead" } */
  cdiag ("bad arg");           /* { dg-warning "misspelled term 'arg' in format; use 'argument' instead" } */
  cdiag ("two args");          /* { dg-warning "misspelled term 'args' in format; use 'arguments' instead" } */
  cdiag ("args 1 and 2");      /* { dg-warning "misspelled term 'args' in format; use 'arguments' instead" } */

  cdiag ("Reg A");             /* { dg-warning "misspelled term 'Reg' in format; use 'register' instead" } */
  cdiag ("regs A and B");      /* { dg-warning "misspelled term 'regs' in format; use 'registers' instead" } */
  cdiag ("no regs");           /* { dg-warning "misspelled term 'regs' in format; use 'registers' instead" } */

  /* Verify words that end in "arg" and "args" or "reg" and "regs" are
     not diagnosed.  */
  cdiag ("gulmarg and balfarg");
  cdiag ("ademargs or toshargs");
  cdiag ("talk to Greg");
  cdiag ("prepreg is a fabric");
  cdiag ("there are dregs in my wine");
}


void test_cdiag_directive (tree t, gimple *gc)
{
  (void)&t; (void)&gc;

  cxxdiag ("%<%s%>", "");     /* { dg-warning "quoted '%s' directive in format" } */
  /* This was asked to be diagnosed in PR #90158 but there, the \"%s\"
     is in parenheses which ends up getting diagnosed because of
     the two consecutive punctuation characters, ( and ".  */
  cdiag ("\"%s\"", "");       /* { dg-warning "quoted '%s' directive in format" } */

  /* Make sure quoted paired tokens are not diagnosed.  */
  cdiag ("%<'%>");
  cdiag ("%<\"%>");
  cdiag ("%<<%>");
  cdiag ("%<>%>");
  cdiag ("%<(%>");
  cdiag ("%<)%>");
  cdiag ("%<[%>");
  cdiag ("%<]%>");

  cdiag ("%<'%> %<\"%> %<>%> %<<%> %<)%> %<(%> %<]%> %<[%>");
}
