typedef unsigned char byte;
typedef unsigned int uint;
typedef unsigned long ulong;
typedef ulong gs_char;
typedef struct gs_show_enum_s gs_show_enum;
typedef struct gs_font_s gs_font;
typedef struct gx_font_stack_item_s {
  gs_font *font;
} gx_font_stack_item;
typedef struct gx_font_stack_s {
  gx_font_stack_item items[1 + 5 ];
} gx_font_stack;
struct gs_show_enum_s {
  gx_font_stack fstack;
};
typedef enum {
  ft_composite = 0,
} font_type;
struct gs_font_s {
  font_type FontType;
};
typedef enum {
  fmap_escape = 3,
  fmap_shift = 8
  } fmap_type;
typedef struct gs_type0_data_s {
  fmap_type FMapType;
} gs_type0_data;
gs_type0_next_char(register gs_show_enum *penum)
{
  const byte *p;
  int fdepth;
  gs_font *pfont;
  gs_type0_data *pdata;
  uint fidx;
  gs_char chr;
  for (; pfont->FontType == ft_composite; )
    {
      fmap_type fmt;
      switch ( fmt )
	{
	  do {} while (0);
	rdown:
	  continue;
	case fmap_shift:
	  p++;
	  do {} while (0);
	  goto rdown;
	}
      break;
    }
 up:
  while ( fdepth > 0 )
    {
      switch ( pdata->FMapType )
	{
	default:
	  continue;
	case fmap_escape:
	  fidx = *++p;
	  do {} while (0);
	  if ( fidx == chr && fdepth > 1 )
	    goto up;
	down:
	  fdepth--;
	  do {} while (0);
	}
      break;
    }
  while ( (pfont = penum->fstack.items[fdepth].font)->FontType == ft_composite )
    ;
}
