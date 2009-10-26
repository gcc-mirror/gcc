/* { dg-lto-do link } */
/* { dg-lto-options {{-fPIC -r -nostdlib -flto} {-fPIC -r -nostdlib -O2 -flto}} } */

void * HeapAlloc(void*,unsigned int,unsigned long);

typedef struct tagGdiFont GdiFont;

typedef struct tagDC {
    int xunused;
    GdiFont *gdiFont;
    unsigned int font_code_page;
} DC;

DC *alloc_dc_ptr( void *funcs, unsigned short magic )
{
  DC *dc;
  if (!(dc = HeapAlloc( 0, 0, sizeof(*dc) ))) return ((void *)0);
  dc->gdiFont = 0;
  return dc;
}

