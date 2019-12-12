/* For PR rtl-optimization/27735  */
/* { dg-do compile } */
/* { dg-options "-O2 -funswitch-loops -fdump-tree-unswitch-details -fno-finite-loops" } */

void set_color(void);
void xml_colorize_line(unsigned int *p, int state)
{
  int c;
  switch(state) 
    {
    case 1:
      goto parse_tag;
    case 2:
      goto parse_comment;
    }

  for(;;) 
    {
      c = *p;  
      if (c == '<' && state == 0) 
	{
parse_comment: ;
	  while (*p != '\n') 
	    state = 3;
parse_tag: ;
	  while (*p != '\n') 
	    state = 0;
	  set_color();
	}
      else
	p++;
    }
}

/* Test that we actually unswitched something.  */
/* { dg-final { scan-tree-dump ";; Unswitching loop" "unswitch" } } */
