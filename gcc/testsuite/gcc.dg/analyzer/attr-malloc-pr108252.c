struct foo
{
  int m_int;
};

extern void foo_release (struct foo *);
extern struct foo *foo_acquire (void)
  __attribute__ ((malloc (foo_release)));

struct {
  /* [...snip...] */
  struct foo *listen_default_ciphers;
  struct foo *connect_default_ciphers;
  /* [...snip...] */
} g;

int parse_global_ciphers(char **args)
{
  struct foo **target;
  target = ((args[0][12] == 'b')
	    ? &g.listen_default_ciphers
	    : &g.connect_default_ciphers);
  *target = foo_acquire ();
  return 0; /* { dg-bogus "leak" } */
}
