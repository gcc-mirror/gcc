extern struct _IO_FILE *stderr;
typedef long integer;
typedef unsigned char byte;
short nl;
byte * tfmfilearray;
integer charbase, ligkernbase;
unsigned char charsonline;
short c;
unsigned short r;
struct {
  short cc;
  integer rr;
} labeltable[259];
short sortptr;
unsigned char activity[(32510) + 1];
integer ai, acti;
extern void _IO_putc (char, struct _IO_FILE *);

void
mainbody (void)
{
  register integer for_end;
  if (c <= for_end)
    do {
      if (((tfmfilearray + 1001)[4 * (charbase + c) + 2] % 4) == 1)
	{
	  if ( r < nl )
	    ;
	  else
	    {
	      while (labeltable[sortptr ].rr > r)
		labeltable[sortptr + 1 ]= labeltable[sortptr];
	    }
	}
    } while (c++ < for_end);

  if (ai <= for_end)
    do {
      if (activity[ai]== 2)
	{
	  r = (tfmfilearray + 1001)[4 * (ligkernbase + (ai))];
	  if (r < 128)
	    {
	      r = r + ai + 1 ;
	      if (r >= nl)
		{
		  if (charsonline > 0)
		    _IO_putc ('\n', stderr);
		}
	    }
	}
    } while (ai++ < for_end);
}
