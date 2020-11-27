#include "rust.h"
#include "y.rs.h"

yystype yylval;
static int sym;
static int __yyerror;
extern int yylineno;

static void yyerror (const char *, ...);
extern int yylex (void);
static bool __yyaccept__ (int, bool);
static bool yyexpect (int);

#ifdef _DEBUG
# define yylex_()                                               \
  yylex ();                                                     \
  do {                                                          \
    char *__token = yytoken_string (sym);                       \
    printf ("[%i:%s]\n", sym, __token);                         \
    free (__token);                                             \
  } while (0)
#else
# define yylex_()  yylex ()
#endif

static std::vector<ALLOCA_> * alloca_modifiers (void);

static rdot type (void);
static rdot target (void);
static rdot suite (void);
static rdot else_block (void);
static rdot elif_block (void);
static rdot if_block (void);
static rdot struct_conditional (void);
static rdot primary (void);
static rdot factor1 (void);
static rdot factor2 (void);
static rdot expression (void);

static vec<rdot, va_gc> * symStack;
static const char * token_strings [] = {
  "impl",
  "as",
  "break",
  "continue",
  "do",
  "fn",
  "let",
  "mut",
  "loop",
  "static",
  "->",
  "bool",
  "int",
  "uint",
  "float",
  "::",
  "enum",
  "==",
  "!=",
  "<",
  ">",
  "<=",
  ">=",
  "struct",
  "while",
  "if",
  "else",
  "self",
  "match",
  "=>",
  "true",
  "false",
  "string_literal",
  "identifier",
  "integer",
  "pub",
  "for",
  "trait",
  "else if",
  "float",
  "unknown"
};

static char *
yytoken_string (int token)
{
  char retval [128];
  memset (retval, 0, 128);
  if (token >= 258 && token <= 298)
    strncpy (retval, token_strings [token-258-1], 128);
  else
    retval [0] = (char) token;
  return xstrdup (retval);
}

#define yyaccept(_X)   __yyaccept__ (_X, true)
#define yyaccept_(_X)  __yyaccept__ (_X, false)
static
bool __yyaccept__ (int s, bool forward)
{
  if (sym == s)
    {
      if (forward)
        {
          sym = yylex_ ();
        }
      return true;
    }
  return false;
}

static
bool yyexpect (int s)
{
  bool retval = false;
  if (yyaccept (s) == true)
    retval = true;
  else
    {
      char * e1 = yytoken_string (s);
      char * e2 = yytoken_string (sym);
      yyerror ("expected [%s] got [%s]", e1, e2);
      free (e1);
      free (e2);
    }
  return retval;
}

/* Also realy need to make use of location_t and gcc diagnostics .. */
/* this is really hacky but its ok for now really need to fix this though */
static void __attribute__ ((format (printf, 1, 2)))
yyerror (const char * fmt, ...)
{
  __yyerror += 1;
  char * buffer = (char *) alloca (512);
  memset (buffer, 0, 512);
  
  va_list vl;
  va_start (vl, fmt);
  vsprintf (buffer, fmt, vl);
  va_end (vl);

  char * buffer_message = (char *) alloca (512);
  memset (buffer_message, 0, 512);
  snprintf (buffer_message, 512, "syntax error at %i: [%s]", yylineno, buffer);
    
  fatal_error ("%s", buffer_message);
}

std::vector<ALLOCA_> * alloca_modifiers (void)
{
  std::vector<ALLOCA_> * allocas = new std::vector<ALLOCA_>;
  while (sym == '~' || sym == '&' || sym == '*')
    {
      ALLOCA_ mod;
      switch (sym)
        {
        case '~':
          mod = ALLOC_HEAP;
          break;
        case '&':
          mod = ALLOC_REF;
          break;
        case '*':
          mod = ALLOC_DEREF;
          break;
        default:
          break;
        }
      yyexpect (sym);
      allocas->push_back (mod);
    }
  return allocas;
}

rdot type (void)
{
  std::vector<ALLOCA_> * mem = alloca_modifiers ();
  rdot retval = NULL_DOT;
  if (yyaccept (TYPE_INT))
    retval = rdot_build_decl1 (RTYPE_INT, NULL_DOT);
  else if (yyaccept (TYPE_UINT))
    retval = rdot_build_decl1 (RTYPE_UINT, NULL_DOT);
  else if (yyaccept (TYPE_FLOAT))
    retval = rdot_build_decl1 (RTYPE_FLOAT, NULL_DOT);
  else if (yyaccept (TYPE_BOOL))
    retval = rdot_build_decl1 (RTYPE_BOOL, NULL_DOT);
  else if (yyaccept (IDENTIFIER))
    {
      char * sid = yylval.string;
      retval = rdot_build_decl1 (RTYPE_USER_STRUCT,
                                 rdot_build_identifier (sid));
      free (sid);
    }
  else
    yyerror ("expected a type got [%s]", yytoken_string (sym));

  RDOT_MMEM_COPY (mem, RDOT_MEM_MODIFIER (retval));
  delete mem;

  return retval;
}

rdot target (void)
{
  yyexpect (LET);
  bool qual = true;
  if (yyaccept (MUT))
    qual = false;
  
  yyexpect (IDENTIFIER);
  char * tid = yylval.string;
  
  rdot ltype = NULL_DOT;
  if (yyaccept (':'))
    ltype = type ();
  if (ltype == NULL_DOT)
      ltype = rdot_build_decl1 (RTYPE_INFER, NULL_DOT);
  
  rdot retval = rdot_build_varDecl (ltype, qual,
                                    rdot_build_identifier (tid));
  free (tid);
  return retval;
}

void argument_list_ (rdot head)
{
  if (yyaccept_ (')'))
    return;

  rdot p = expression ();
  if (head == NULL_DOT)
    vec_safe_push (symStack, p);
  else
    RDOT_CHAIN (head) = p;
  
  if (yyaccept (','))
    argument_list_ (p);
}

rdot argument_list ()
{
  rdot retval = NULL_DOT;
  size_t prev = symStack->length ();
  argument_list_ (NULL_DOT);
  size_t next = symStack->length ();
  
  if (next > prev)
    retval = symStack->pop ();
  return retval;
}

rdot struct_elem ()
{
  yyexpect (IDENTIFIER);
  char * selem = yylval.string;
  yyexpect (':');
  rdot expr = expression ();
  rdot retval = rdot_build_decl2 (D_STRUCT_PARAM,
                                  rdot_build_identifier (selem),
                                  expr);
  free (selem);
  return retval;
}

void struct_init_list_ (rdot head)
{
  if (yyaccept_ ('}'))
    return;
  rdot e = struct_elem ();
  if (head == NULL_DOT)
    vec_safe_push (symStack, e);
  else
    RDOT_CHAIN (head) = e;
  if (yyaccept (','))
    struct_init_list_ (e);
}

rdot struct_init_list (void)
{
  rdot retval = NULL_DOT;
  size_t prev = symStack->length ();
  struct_init_list_ (NULL_DOT);
  size_t next = symStack->length ();
  
  if (next > prev)
    retval = symStack->pop ();
  return retval;
}

rdot primary (void)
{
  rdot retval = NULL_DOT;
  std::vector<ALLOCA_> * mem = alloca_modifiers ();
  if (yyaccept (IDENTIFIER))
    {
      // maybe call...
      char * pid = yylval.string;
      if (yyaccept_ ('('))
        {
          yyexpect ('(');
          rdot alist = argument_list ();
          yyexpect (')');
          retval = rdot_build_decl2 (D_CALL_EXPR,
                                     rdot_build_identifier (pid),
                                     alist);
        }
      // struct init
      else if (yyaccept_ ('{'))
        {
          yyexpect ('{');
          rdot sls = struct_init_list ();
          yyexpect ('}');
          retval = rdot_build_decl2 (D_STRUCT_INIT,
                                     rdot_build_identifier (pid),
                                     sls);
        }
      // just a simple identifier...
      else
        retval = rdot_build_identifier (pid);
      free (pid);
    }
  else if (yyaccept (INTEGER))
    retval = rdot_build_integer (yylval.integer);
  else if (yyaccept (FLOAT))
    retval = rdot_build_float (yylval.ffloat);
  else if (yyaccept (STRING))
    retval = rdot_build_string (yylval.string);
  else if (yyaccept (XFALSE))
    retval = rdot_build_bool (false);
  else if (yyaccept (XTRUE))
    retval = rdot_build_bool (true);
  else
    yyerror ("expected a primary got [%s]", yytoken_string (sym));

  RDOT_MMEM_COPY (mem, RDOT_MEM_MODIFIER (retval));
  delete mem;
  return retval;
}

rdot factor1 (void)
{
  rdot retval = NULL_DOT;
  if (yyaccept (IDENTIFIER))
    {
      char * pid = yylval.string;
      if (yyaccept ('='))
        {
          rdot rhs = expression ();
          retval = rdot_build_decl2 (D_MODIFY_EXPR, rdot_build_identifier (pid), rhs);
        }
      else if (yyaccept_ ('('))
        {
          yyexpect ('(');
          rdot alist = argument_list ();
          yyexpect (')');
          retval = rdot_build_decl2 (D_CALL_EXPR,
                                     rdot_build_identifier (pid),
                                     alist);
        }
      // struct init
      else if (yyaccept_ ('{'))
        {
          yyexpect ('{');
          rdot sls = struct_init_list ();
          yyexpect ('}');
          retval = rdot_build_decl2 (D_STRUCT_INIT,
                                     rdot_build_identifier (pid),
                                     sls);
        }
      else if (yyaccept_ (ACC))
	{
	  yyexpect (ACC);
	  rdot node = factor1 ();
	  retval = rdot_build_decl2 (D_ACC_EXPR, rdot_build_identifier (pid), node);
	}
      else
        {
          retval = rdot_build_identifier (yylval.string);
          if (yyaccept ('.'))
            {
              rdot rhs = factor2 ();
              retval = rdot_build_decl2 (D_ATTRIB_REF, retval, rhs);
            }
        }
      free (pid);
    }
  else
    retval = factor2 ();
  return retval;
}

rdot factor2 (void)
{
  rdot retval = NULL_DOT;
  if (yyaccept ('('))
    {
      retval = expression ();
      yyexpect (')');
    }
  else
    {
      retval = primary ();
      if (RDOT_TYPE (retval) == D_IDENTIFIER
          || RDOT_TYPE (retval) == D_CALL_EXPR)
        {
          if (yyaccept ('.'))
            {
              rdot rhs = factor2 ();
              retval = rdot_build_decl2 (D_ATTRIB_REF, retval, rhs);
            }
        }
    }
  return retval;
}

opcode_t symToDeclType (int sym)
{
  opcode_t retval = D_D_EXPR;
  switch (sym)
    {
    case '=':
      retval = D_MODIFY_EXPR;
      break;
      
    case '+':
      retval = D_ADD_EXPR;
      break;

    case '-':
      retval = D_MINUS_EXPR;
      break;

    case '*':
      retval = D_MULT_EXPR;
      break;

    case '/':
      retval = D_DIVD_EXPR;
      break;

    case '.':
      retval = D_ATTRIB_REF;
      break;

    case EQUAL_EQUAL:
      retval = D_EQ_EQ_EXPR;
      break;

    case NOT_EQUAL:
      retval = D_NOT_EQ_EXPR;
      break;

    case '<':
      retval = D_LESS_EXPR;
      break;
      
    case LESS_EQUAL:
      retval = D_LESS_EQ_EXPR;
      break;

    case '>':
      retval = D_GREATER_EXPR;
      break;

    case GREATER_EQUAL:
      retval = D_GREATER_EQ_EXPR;
      break;
      
    default:
      yyerror ("invalid symbol [%i:%s]",
               sym, yytoken_string (sym));
      break;
    }
  return retval;
}

rdot expression (void)
{
  bool head = false;
  rdot retval = factor1 ();
  rdot next = NULL_DOT;
  while (sym == '+' || sym == '-' ||
         sym == '*' || sym == '/' ||
         sym == '<' || sym == '>' ||
         sym == EQUAL_EQUAL || sym == NOT_EQUAL ||
         sym == LESS_EQUAL || sym == GREATER_EQUAL)
    {
      opcode_t o = symToDeclType (sym);
      yyexpect (sym);
      rdot rhs = factor2 ();
      if (head == false)
        {
          retval = next = rdot_build_decl2 (o, retval, rhs);
          head = true;
        }
      else
        {
          rdot prev = RDOT_rhs_TT (next);
          rdot rhs_expr = rdot_build_decl2 (o, prev, rhs);
          RDOT_rhs_TT (next) = rhs_expr;
          next = rhs_expr;
        }
    }
  return retval;
}

rdot statement ()
{
  rdot retval = NULL_DOT;
  if (yyaccept_ (BREAK))
    {
      yyexpect (BREAK);
      retval = rdot_build_decl1 (C_BREAK_STMT, NULL_DOT);
    }
  else if (yyaccept_ (CONTINUE))
    {
      yyexpect (CONTINUE);
      retval = rdot_build_decl1 (C_CONT_STMT, NULL_DOT);
    }
  else if (yyaccept_ (RETURN))
    {
      yyexpect (RETURN);
      retval = rdot_build_decl1 (C_RETURN_STMT, expression ());
    }
  else if (yyaccept_ (LET))
    {
      // simple vardecl [let x;]
      rdot tg = target ();
      if (yyaccept ('='))
        {
          retval = rdot_build_decl2 (D_MODIFY_EXPR,
                                     tg, expression ());
        }
      else
        retval = tg;
    }
  else
      retval = expression ();
  return retval;
}

rdot struct_while_loop ()
{
  yyexpect (WHILE);
  rdot expr = expression ();
  yyexpect ('{');
  rdot sb = suite ();
  yyexpect ('}');
  return rdot_build_decl2 (D_STRUCT_WHILE, expr, sb);
}

rdot struct_loop ()
{
  yyexpect (LOOP);
  yyexpect ('{');
  rdot sb = suite ();
  yyexpect ('}');
  return rdot_build_decl1 (D_STRUCT_LOOP, sb);
}

rdot if_block (void)
{
  yyexpect (IF);
  rdot expr = expression ();
  yyexpect ('{');
  rdot sb = suite ();
  yyexpect ('}');
  return rdot_build_decl2 (D_STRUCT_IF, expr, sb);
}

rdot elif_block (void)
{
  yyexpect (ELIF);
  rdot expr = expression ();
  yyexpect ('{');
  rdot sb = suite ();
  yyexpect ('}');
  return rdot_build_decl2 (D_STRUCT_IF, expr, sb);
}

rdot else_block (void)
{
  yyexpect (ELSE);
  yyexpect ('{');
  rdot block = suite ();
  yyexpect ('}');
  return rdot_build_decl1 (D_STRUCT_ELSE, block);
}

rdot struct_conditional ()
{
  rdot iblock = if_block ();
  rdot eblock = NULL_DOT;
  rdot elblock = NULL_DOT;
  rdot curr = NULL_DOT;
  rdot prev = NULL_DOT;
  while (yyaccept_ (ELIF))
    {
      curr = elif_block ();
      if (elblock == NULL_DOT)
        elblock = prev = curr;
      else
        {
          RDOT_CHAIN (prev) = curr;
          prev = curr;
        }
    }
  if (yyaccept_ (ELSE))
    eblock = else_block ();
  rdot retval = rdot_build_decl2 (D_STRUCT_IF, iblock, eblock);
  RDOT_FIELD (retval)= elblock;
  return retval;
}

void statement_list (rdot head)
{
  if (yyaccept_ ('}'))
    return;

  rdot st = NULL_DOT;
  if (yyaccept_ (LOOP))
    st = struct_loop ();
  else if (yyaccept_ (WHILE))
    st = struct_while_loop ();
  else if (yyaccept_ (IF))
    st = struct_conditional (); 
 else
    {
      st = statement ();
      if (!yyaccept (';'))
        DOT_RETVAL (st) = true;
    }
  gcc_assert (st != NULL_DOT);
  if (head == NULL_DOT)
    vec_safe_push (symStack, st);
  else
    RDOT_CHAIN (head) = st;
  statement_list (st);
}

rdot suite ()
{
  rdot retval = NULL_DOT;
  size_t prev = symStack->length ();
  statement_list (NULL);
  size_t next = symStack->length ();
  
  if (next > prev)
    retval = symStack->pop ();
  return retval;
}

rdot param (void)
{
  yyexpect (IDENTIFIER);
  char * pid = yylval.string;
  yyexpect (':');
  rdot pit = type ();
  rdot retval = rdot_build_decl2 (D_PARAMETER,
                                  rdot_build_identifier (pid),
                                  pit);
  free (pid);
  return retval;
}

void param_list_ (rdot head)
{
  if (yyaccept_ (')'))
    return;

  rdot p = param ();
  if (head == NULL_DOT)
    vec_safe_push (symStack, p);
  else
    RDOT_CHAIN (head) = p;
  
  if (yyaccept (','))
    param_list_ (p);
}

rdot param_list ()
{
  rdot retval = NULL_DOT;
  size_t prev = symStack->length ();
  param_list_ (NULL_DOT);
  size_t next = symStack->length ();
  
  if (next > prev)
    retval = symStack->pop ();
  return retval;
}

/* fndecl := [pub] fn IDENTIFIER ([param_list]) [-> type] { stmt_list } */
rdot fndecl ()
{
  bool pub = false;
  if (yyaccept (PUB))
    pub = true;
  yyexpect (DEFUN);
  yyexpect (IDENTIFIER);
  char * fid = yylval.string;
  yyexpect ('(');
  rdot plist = param_list ();
  yyexpect (')');
  rdot rtype = NULL_DOT;
  if (yyaccept (RTYPE))
    rtype = type ();
  yyexpect ('{');
  rdot block = suite ();
  yyexpect ('}');
  rdot retval =  rdot_build_fndecl (rdot_build_identifier (fid),
                                    pub, plist, rtype, block);
  free (fid);
  return retval;
}

void struct_layout_ (rdot head)
{
  if (yyaccept_ ('}'))
    return;
   
  rdot p = param ();
  if (head == NULL_DOT)
    vec_safe_push (symStack, p);
  else
    RDOT_CHAIN (head) = p;
  
  if (yyaccept (','))
    struct_layout_ (p);
}

rdot struct_layout ()
{
  rdot retval = NULL_DOT;
  size_t prev = symStack->length ();
  struct_layout_ (NULL_DOT);
  size_t next = symStack->length ();
  
  if (next > prev)
    retval = symStack->pop ();
  return retval;
}

rdot struct_decl ()
{
  yyexpect (STRUCT);
  yyexpect (IDENTIFIER);
  char * sid = yylval.string;
  
  yyexpect ('{');
  rdot layout = struct_layout ();
  yyexpect ('}');
  rdot retval = rdot_build_decl2 (D_STRUCT_TYPE,
                                  rdot_build_identifier (sid),
                                  layout);
  free (sid);
  return retval;
}

void fndecl_block_ (rdot head)
{
  if (yyaccept_ ('}'))
    return;
  
  rdot f = fndecl ();
  if (head == NULL_DOT)
    vec_safe_push (symStack, f);
  else
    RDOT_CHAIN (head) = f;

  fndecl_block_ (f);
}

rdot fndecl_block (void)
{
  rdot retval = NULL_DOT;
  size_t prev = symStack->length ();
  fndecl_block_ (NULL_DOT);
  size_t next = symStack->length ();
  
  if (next > prev)
    retval = symStack->pop ();
  return retval;
}

rdot impl_block ()
{
  rdot retval = NULL_DOT;
  yyexpect (IMPL);
  yyexpect (IDENTIFIER);
  char * iid = yylval.string;
  rdot rid = rdot_build_identifier (iid);
  free (iid);
  if (yyaccept (FOR))
    yyexpect (IDENTIFIER);
  yyexpect ('{');
  rdot fb = fndecl_block ();
  yyexpect ('}');
  retval = rdot_build_decl2 (D_STRUCT_IMPL, rid, fb);
  return retval;
}

/*
  decl := fndecl | structdecl | impldecl | enumdecl
*/
void decl (void)
{
  rdot rdecl = NULL_DOT;
  if (yyaccept_ (STRUCT))
    rdecl = struct_decl ();
  else if (yyaccept_ (IMPL))
    rdecl = impl_block ();
  else
    rdecl = fndecl ();
  if (rdecl != NULL_DOT)
    dot_pass_pushDecl (rdecl);
}

int yyparse (void)
{
  // kick things off with the first token
  __yyerror = 0;
  sym = yylex_ ();
  
  vec_alloc (symStack, 0);
  vec_safe_push (symStack, NULL_DOT);
  
  while (sym != 0)
    decl ();
  
  if (symStack->length() > 1)
    yyerror ("some unpoped symbols on the parse stack!");
    
  vec_free (symStack);
  return __yyerror;
}
