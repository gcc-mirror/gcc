/* PR target/12898
   0x8000 needs multiple instructions to be emitted on Alpha; the
   fluff around it causes it to be emitted in a no_new_pseudos
   context, which triggered a problem in alpha.c.  */

void f (const char *, ...);
int g (void);
void *p (void);

int isymBase, ilineBase, sym_hdr, want_line, proc_desc;
char *lines;

void print_file_desc (int *fdp)
{
  char *str_base = p ();
  int symi, pdi = g ();

  for (symi = 0; isymBase;)
    {
      int proc_ptr = proc_desc + pdi;
      f("1", isymBase, proc_ptr + *fdp, str_base);
      if (want_line && *fdp)
	{
	  int delta;
	  long cur_line = proc_ptr;
	  char *line_ptr = lines + proc_ptr;
	  char *line_end = p ();

	  f("2", sym_hdr);
	  while (line_ptr < line_end)
	    {
	      delta = *line_ptr;
	      if (delta)
		line_ptr++;
	      else
		delta = line_ptr[1] ^ 0x8000;
	      f("3", cur_line, delta);
	    }
	}
    }
}
