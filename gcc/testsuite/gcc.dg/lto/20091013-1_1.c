typedef struct HDC__ { int unused; } *HDC;
typedef struct HFONT__ { int unused; } *HFONT;

typedef struct
{
  unsigned int ciACP;
} CHARSETINFO, *PCHARSETINFO, *LPCHARSETINFO;

typedef struct tagTEXTMETRICW
{
    int tmCharSet;
} TEXTMETRICW, *LPTEXTMETRICW, *PTEXTMETRICW;

struct gdi_obj_funcs
{
    void* (*pSelectObject)( void* handle, void* hdc );
};

typedef struct tagGdiFont GdiFont;

typedef struct tagDC
{
    int xunused;
    GdiFont *gdiFont;
    unsigned int font_code_page;
} DC;

extern GdiFont* WineEngCreateFontInstance(DC*, HFONT);
extern unsigned int WineEngGetTextCharsetInfo(GdiFont *font, void* fs, unsigned int flags);
extern int WineEngGetTextMetrics(GdiFont*, LPTEXTMETRICW);
extern void* alloc_gdi_handle( void *obj, unsigned short type, const struct gdi_obj_funcs *funcs );

enum __wine_debug_class
{
    __WINE_DBCL_FIXME,
    __WINE_DBCL_ERR,
    __WINE_DBCL_WARN,
    __WINE_DBCL_TRACE,

    __WINE_DBCL_INIT = 7
};

struct __wine_debug_channel
{
    unsigned char flags;
    char name[15];
};

extern int wine_dbg_log( enum __wine_debug_class cls, struct __wine_debug_channel *ch, const char *func,
                         const char *format, ... ) __attribute__((format (printf,4,5)));

static struct __wine_debug_channel __wine_dbch_font = { ~0, "font" };
static struct __wine_debug_channel * const __wine_dbch___default = &__wine_dbch_font;

static void* FONT_SelectObject( void* handle, void* hdc );

static const struct gdi_obj_funcs font_funcs =
{
    FONT_SelectObject,
};

HFONT CreateFontIndirectW( const void *plf )
{
    return alloc_gdi_handle( 0, 6, &font_funcs );
}

static void update_font_code_page( DC *dc )
{
    CHARSETINFO csi;
    int charset = (unsigned char)1;

    if (dc->gdiFont)
        charset = WineEngGetTextCharsetInfo( dc->gdiFont, ((void *)0), 0 );

    if (TranslateCharsetInfo( ((void *)(unsigned long)((unsigned long)charset)), &csi, 1) )
        dc->font_code_page = csi.ciACP;
    else {
        switch(charset) {
        case (unsigned char)1:
            dc->font_code_page = GetACP();
            break;

        case (unsigned char)246:
            dc->font_code_page = 0;
            break;

        default:
            do { if((((__wine_dbch___default))->flags & (1 << __WINE_DBCL_FIXME))) { struct __wine_debug_channel * const __dbch = (__wine_dbch___default); const enum __wine_debug_class __dbcl = __WINE_DBCL_FIXME; wine_dbg_log( __dbcl, __dbch, __FUNCTION__, "Can't find codepage for charset %d\n", charset); } } while(0);
            dc->font_code_page = 0;
            break;
        }
    }

    do { if((((__wine_dbch___default))->flags & (1 << __WINE_DBCL_TRACE))) { struct __wine_debug_channel * const __dbch = (__wine_dbch___default); const enum __wine_debug_class __dbcl = __WINE_DBCL_TRACE; wine_dbg_log( __dbcl, __dbch, __FUNCTION__, "charset %d => cp %d\n", charset, dc->font_code_page); } } while(0);
}

static void* FONT_SelectObject( void* handle, void* hdc )
{
    DC *dc;

    dc->gdiFont = WineEngCreateFontInstance( dc, handle );
    update_font_code_page( dc );
    return 0;
}

int GetTextMetricsW( HDC hdc, TEXTMETRICW *metrics )
{
    DC * dc;
    return WineEngGetTextMetrics(dc->gdiFont, metrics);
}

