typedef long unsigned int size_t;
typedef unsigned char Bufbyte;
typedef int Bytecount;
typedef int Charcount;
typedef struct lstream Lstream;
typedef int  Lisp_Object;
extern Lisp_Object Qnil;
extern inline  int
TRUE_LIST_P (Lisp_Object object)
{
  return ((  object  ) == (  Qnil ))  ;
}
struct Lisp_String
{
  Bytecount _size;
  Bufbyte *_data;
};
typedef enum lstream_buffering
{
  LSTREAM_LINE_BUFFERED,
} Lstream_buffering;
struct lstream
{
  Lstream_buffering buffering;  
  unsigned char *out_buffer;  
  size_t out_buffer_size;  
  size_t out_buffer_ind;  
  size_t byte_count;
  long flags;   
  char data[1];
};
typedef struct printf_spec printf_spec;
struct printf_spec
{
};
typedef union printf_arg printf_arg;
union printf_arg
{
};
typedef struct
{
   int cur;
} printf_spec_dynarr;
typedef struct
{
} printf_arg_dynarr;
static void
doprnt_1 (Lisp_Object stream, const  Bufbyte *string, Bytecount len,
	  Charcount minlen, Charcount maxlen, int minus_flag, int zero_flag)
{
  Charcount cclen;
  Bufbyte pad;
  Lstream *lstr = ((  struct lstream  *) ((void *)((((    stream    ) & ((1UL << ((4   * 8 )  - 4 ) ) - 1UL) ) ) | 0x40000000 )) )  ;
  cclen = (  len ) ;
  if (zero_flag)
    pad = '0';
  pad = ' ';
#if 0
  if (minlen > cclen && !minus_flag)
#endif
    {
      int to_add = minlen - cclen;
      while (to_add > 0)
	{
	  (( lstr )->out_buffer_ind >= ( lstr )->out_buffer_size ?	Lstream_fputc ( lstr ,   pad ) :	(( lstr )->out_buffer[( lstr )->out_buffer_ind++] =	(unsigned char) (  pad ),	( lstr )->byte_count++,	( lstr )->buffering == LSTREAM_LINE_BUFFERED &&	( lstr )->out_buffer[( lstr )->out_buffer_ind - 1] == '\n' ?	Lstream_flush_out ( lstr ) : 0)) ;
	  to_add--;
	}
    }
  if (maxlen >= 0)
    len = (  ((( maxlen ) <= (  cclen )) ? ( maxlen ) : (  cclen ))  ) ;
  Lstream_write (lstr, string, len);
  if (minlen > cclen && minus_flag)
    {
      int to_add = minlen - cclen;
      while (to_add > 0)
	{
	  (( lstr )->out_buffer_ind >= ( lstr )->out_buffer_size ?	Lstream_fputc ( lstr ,   pad ) :	(( lstr )->out_buffer[( lstr )->out_buffer_ind++] =	(unsigned char) (  pad ),	( lstr )->byte_count++,	( lstr )->buffering == LSTREAM_LINE_BUFFERED &&	( lstr )->out_buffer[( lstr )->out_buffer_ind - 1] == '\n' ?	Lstream_flush_out ( lstr ) : 0)) ;
	  to_add--;
	}
    }
}
static Bytecount
emacs_doprnt_1 (Lisp_Object stream, const  Bufbyte *format_nonreloc,
		Lisp_Object format_reloc, Bytecount format_length,
		int nargs,
		const  Lisp_Object *largs)
{
  int i;
  printf_spec_dynarr *specs = 0;
  format_nonreloc = (( ((  struct Lisp_String  *) ((void *)((((     format_reloc     ) & ((1UL << ((4   * 8 )  - 4 ) ) - 1UL) ) ) | 0x40000000 )) )   )->_data + 0)  ;
  format_length = (( ((  struct Lisp_String  *) ((void *)((((     format_reloc     ) & ((1UL << ((4   * 8 )  - 4 ) ) - 1UL) ) ) | 0x40000000 )) )   )->_size)  ;
  specs = parse_doprnt_spec (format_nonreloc, format_length);
  for (i = 0; i < (( specs )->cur) ; i++)
    {
      char ch;
      doprnt_1 (stream, (Bufbyte *) &ch, 1, 0, -1, 0, 0);
    }
}
