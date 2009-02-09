/* PR middle-end/38981 */
/* Reporter: Kamaraju Kusumanchi <kamaraju@gmail.com> */

struct d_info
{
  int **subs;
};

static int *
d_substitution (struct d_info *di, int prefix)
{
  char c;

	c='_';

  if (c == '_')
    {
      unsigned int id;

      if (c != '_')
	{
	  do
	    {
	      unsigned int new_id;

	      if (new_id < id)
		return 0;
	      id = new_id;
	    }
	  while (c != '_');
	}



      return di->subs[id];
    }
  else
    {
      int verbose;
      int code;
      int simple_len;

	code=0;
	simple_len=0;
	verbose=0;
      if (! code && prefix)
	{
	  char peek;
		peek='A';

	  if (peek == 'C' || peek == 'D')
	    verbose = 1;
	}

	      if (verbose)
		{
		  code = simple_len;
		}

    }
}
