extern char *strdup (const char *__s)
  __attribute__ ((__nothrow__ , __leaf__, __malloc__, __nonnull__ (1)));

struct {
  /* [...snip...] */
  char *listen_default_ciphers;
  char *connect_default_ciphers;
  /* [...snip...] */
} g;

int parse_global_ciphers(char **args)
{
  char **target;
  target = ((args[0][12] == 'b')
	    ? &g.listen_default_ciphers
	    : &g.connect_default_ciphers);
  *target = strdup(args[1]);
  return 0; /* { dg-bogus "leak" } */
}
