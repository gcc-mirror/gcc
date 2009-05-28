typedef __SIZE_TYPE__ size_t;
struct re_pattern_buffer
  {
    unsigned char *buffer;
    unsigned long int used;
  };
struct re_registers
  {
    int *start;
  };

static const char **regstart, **regend;
static const char **old_regend;

static int
re_match_2_internal (struct re_pattern_buffer *bufp,
		     struct re_registers *regs)
{
  unsigned char *p = bufp->buffer;
  unsigned char *pend = p + bufp->used;

  for (;;)
    {
      int highest_active_reg = 1;
      if (bufp)
	{
	  int i;
	  for (i = 1;; i++)
	    regs->start[i] = 0;
	}

      switch ((unsigned int) *p++)
	{
	case 1:
	  {
	    unsigned char r = *p;
	    if (r)
	      highest_active_reg = r;
	  }
	  if (p + 2 == pend)
	    {
	      char is_a_jump_n = 0;
	      int mcnt = 0;
	      unsigned char *p1;

	      p1 = p + 2;
	      switch (*p1++)
		{
		case 2:
		  is_a_jump_n = 1;
		case 1:
		  do { do { mcnt = *p1; } while (0); p1 += 2; } while (0);
		  if (is_a_jump_n)
		    p1 = 0;
		}

	      if (mcnt && *p1 == 0)
		{
		  unsigned r;
		  for (r = 0; r < (unsigned) *p + (unsigned) *(p + 1); r++)
		    {
		      if (regend[0] >= regstart[r])
			regend[r] = old_regend[r];
		    }
		  do { while (0 < highest_active_reg + 1) { } } while (0);
		}
	    }
	}
    }

  return -1;
}
