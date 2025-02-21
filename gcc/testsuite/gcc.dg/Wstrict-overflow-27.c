/* { dg-do compile } */
/* { dg-options "-O3 -Wstrict-overflow=2 -Werror" } */

typedef __SIZE_TYPE__ size_t;
extern char *strtok_r (char *__restrict __s, const char *__restrict __delim,
		       char **__restrict __save_ptr)
  __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2, 3)));
extern const unsigned short int **__ctype_b_loc (void)
  __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));
extern int *DEBUGLEVEL_CLASS;
size_t debug_num_classes = 0;
void debug_parse_param(char *param);
void
debug_parse_levels(const char *params_str, size_t str_len)
{
  char str[str_len+1];
  char *tok, *saveptr;
  size_t i;
  tok = strtok_r(str, " \t,\n\r", &saveptr);
  if (((*__ctype_b_loc ())[(int) ((tok[0]))]))
    tok = strtok_r(((void *)0), " \t,\n\r", &saveptr);
  else
    DEBUGLEVEL_CLASS[0] = 0;
  for (i = 0 +1; i < debug_num_classes; i++)
    DEBUGLEVEL_CLASS[i] = DEBUGLEVEL_CLASS[0];
  while (tok != ((void *)0) )
    debug_parse_param(tok);
}
