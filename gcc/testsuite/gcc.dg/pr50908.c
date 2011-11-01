/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -ftree-tail-merge" } */

enum Lisp_Type
{
  Lisp_Int0 = 0, Lisp_Int1 = 4, Lisp_Symbol = 2, Lisp_Misc = 3, Lisp_String =
    1, Lisp_Vectorlike = 5, Lisp_Cons = 6, Lisp_Float = 7,
};
typedef long Lisp_Object;
enum pvec_type
{
  PVEC_NORMAL_VECTOR = 0, PVEC_PROCESS = 0x200, PVEC_FRAME =
    0x400, PVEC_COMPILED = 0x800, PVEC_WINDOW =
    0x1000, PVEC_WINDOW_CONFIGURATION = 0x2000, PVEC_SUBR =
    0x4000, PVEC_CHAR_TABLE = 0x8000, PVEC_BOOL_VECTOR =
    0x10000, PVEC_BUFFER = 0x20000, PVEC_HASH_TABLE = 0x40000, PVEC_TERMINAL =
    0x80000, PVEC_SUB_CHAR_TABLE = 0x100000, PVEC_FONT =
    0x200000, PVEC_OTHER = 0x400000, PVEC_TYPE_MASK = 0x7ffe00
};
struct Lisp_Vector
{
  unsigned long size;
};
struct Lisp_Char_Table
{
  Lisp_Object defalt;
  Lisp_Object ascii;
};
struct Lisp_Sub_Char_Table
{
  Lisp_Object contents[1];
};
extern Lisp_Object Qnil, Qt, Qquote, Qlambda, Qsubr, Qunbound;
struct buffer_text
{
  unsigned char *beg;
  long gpt_byte;
  long gap_size;
};
struct buffer
{
  struct buffer_text *text;
  struct region_cache *width_run_cache;
  Lisp_Object tab_width;
  Lisp_Object ctl_arrow;
};
extern struct buffer *current_buffer;
extern Lisp_Object Vchar_width_table;
struct frame
{
  long text_lines, text_cols;
};
struct window
{
  Lisp_Object frame;
};
extern Lisp_Object Vtruncate_partial_width_windows;
extern struct Lisp_Char_Table *window_display_table (struct window *);
struct position *
compute_motion (from, fromvpos, fromhpos, did_motion, to, tovpos, tohpos,
		width, hscroll, tab_offset, win)
     long from, fromvpos, fromhpos, to, tovpos, tohpos;
     struct window *win;
{
  register long hpos = fromhpos;
  register long pos;
  long pos_byte;
  register int c = 0;
  register struct Lisp_Char_Table *dp = window_display_table (win);
  long wide_column_end_hpos = 0;
  long continuation_glyph_width;
  while (1)
    {
      if (hpos > width)
	{
	  int total_width = width + continuation_glyph_width;
	  if (!((Vtruncate_partial_width_windows) == (Qnil))
	      && (total_width <
		  (((void) 0,
		    (struct frame
		     *) ((long) (((win)->frame) & ~((((long) 1) << 3) -
						    1)))))->text_cols))
	    {
	      if (pos <= to)
		{
		  pos = find_before_next_newline (pos, to, 1);
		}
	      if (wide_column_end_hpos > width)
		{
		  hpos -= width;
		}
	    }
	}
      else
	{
	  Lisp_Object charvec;
	  c =
	    *(((((pos_byte)) >=
		(current_buffer->text->gpt_byte) ? (current_buffer->text->
						    gap_size) : 0) +
	       ((pos_byte)) + (current_buffer->text->beg) - ((1))));
	  if (current_buffer->width_run_cache)
	    {
	      if (((((enum Lisp_Type) (((unsigned long) ((charvec))) &
				       ((((long) 1) << 3) - 1))) ==
		    Lisp_Vectorlike)
		   &&
		   !(((void) 0,
		      (struct Lisp_Vector
		       *) ((long) ((charvec) & ~((((long) 1) << 3) - 1))))->
		     size & ((((unsigned long) 1 << (64 - 1)) >> 1)))))
		{
		  unsigned char *ptr;
		  int bytes, width, wide_column;
		  do
		    {
		      if ((!((*ptr) & 0x80) ? 1 : !((*ptr) & 0x20) ? 2 :
			   !((*ptr) & 0x10) ? 3 : !((*ptr) & 0x08) ? 4 : 5) !=
			  bytes)
			width = bytes * 4;
		      else
			{
			  if (dp != 0
			      &&
			      ((((enum
				  Lisp_Type) (((unsigned
						long) (((((unsigned) (c) <
							  0x80)
							 ? ((((dp)->ascii) ==
							     (Qnil)) ? (dp)->
							    defalt
							    : (((((enum
								   Lisp_Type)
								  (((unsigned
								     long) (((dp)->ascii))) & ((((long) 1) << 3) - 1))) == Lisp_Vectorlike) && (((((void) 0, (struct Lisp_Vector *) ((long) (((dp)->ascii) & ~((((long) 1) << 3) - 1))))->size & (((((unsigned long) 1 << (64 - 1)) >> 1)) | (PVEC_SUB_CHAR_TABLE)))) == (((((unsigned long) 1 << (64 - 1)) >> 1)) | (PVEC_SUB_CHAR_TABLE)))) ? ((void) 0, (struct Lisp_Sub_Char_Table *) ((long) (((dp)->ascii) & ~((((long) 1) << 3) - 1))))->contents[c] : (dp)->ascii)) : disp_char_vector ((dp), (c)))))) & ((((long) 1) << 3) - 1))) == Lisp_Vectorlike) && !(((void) 0, (struct Lisp_Vector *) ((long) (((((unsigned) (c) < 0x80) ? ((((dp)->ascii) == (Qnil)) ? (dp)->defalt : (((((enum Lisp_Type) (((unsigned long) (((dp)->ascii))) & ((((long) 1) << 3) - 1))) == Lisp_Vectorlike) && (((((void) 0, (struct Lisp_Vector *) ((long) (((dp)->ascii) & ~((((long) 1) << 3) - 1))))->size & (((((unsigned long) 1 << (64 - 1)) >> 1)) | (PVEC_SUB_CHAR_TABLE)))) == (((((unsigned long) 1 << (64 - 1)) >> 1)) | (PVEC_SUB_CHAR_TABLE)))) ? ((void) 0, (struct Lisp_Sub_Char_Table *) ((long) (((dp)->ascii) & ~((((long) 1) << 3) - 1))))->contents[c] : (dp)->ascii)) : disp_char_vector ((dp), (c)))) & ~((((long) 1) << 3) - 1))))->size & ((((unsigned long) 1 << (64 - 1)) >> 1)))))
			    width =
			      ((void) 0,
			       (struct Lisp_Vector
				*) ((long) (((((unsigned) (c) <
					       0x80) ? ((((dp)->ascii) ==
							 (Qnil)) ? (dp)->
							defalt
							: (((((enum
							       Lisp_Type) (((unsigned long) (((dp)->ascii))) & ((((long) 1) << 3) - 1))) == Lisp_Vectorlike) && (((((void) 0, (struct Lisp_Vector *) ((long) (((dp)->ascii) & ~((((long) 1) << 3) - 1))))->size & (((((unsigned long) 1 << (64 - 1)) >> 1)) | (PVEC_SUB_CHAR_TABLE)))) == (((((unsigned long) 1 << (64 - 1)) >> 1)) | (PVEC_SUB_CHAR_TABLE)))) ? ((void) 0, (struct Lisp_Sub_Char_Table *) ((long) (((dp)->ascii) & ~((((long) 1) << 3) - 1))))->contents[c] : (dp)->ascii)) : disp_char_vector ((dp), (c)))) & ~((((long) 1) << 3) - 1))))->size;
			  else
			    width =
			      (((unsigned) (c) < 0x80) ? (c <
							  0x20 ? (c ==
								  '\t'
								  ? ((((long)
								       (current_buffer->
									tab_width))
								      >> (3 -
									  1)))
								  : (c ==
								     '\n' ? 0
								     : (((current_buffer->ctl_arrow) == (Qnil)) ? 4 : 2))) : (c < 0x7f ? 1 : ((((current_buffer->ctl_arrow) == (Qnil)) ? 4 : 2)))) : (((long) ((((unsigned) (c) < 0x80) ? (
																														       {
																														       Lisp_Object
																														       _val;
																														       _val;}
			):																							char_table_ref ((Vchar_width_table), (c))))) >> (3 - 1)));
			  if (width > 1)
			    wide_column = width;
			}
		    }
		  while (0);
		  if (wide_column)
		    wide_column_end_hpos = hpos + wide_column;
		}
	    }
	}
    }
}
