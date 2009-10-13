typedef struct HDC__ { int unused; } *HDC;
typedef struct HFONT__ { int unused; } *HFONT;

void* HeapAlloc(void*,unsigned int,unsigned long);

typedef struct tagLOGFONTW
{
    int lfPitchAndFamily;
    unsigned short lfFaceName[32];
} LOGFONTW, *PLOGFONTW, *LPLOGFONTW;

typedef struct tagGdiFont GdiFont;
typedef struct tagDC DC;

extern unsigned int WineEngGetFontData(GdiFont*, unsigned int, unsigned int, void*, unsigned int);

struct list
{
    struct list *next;
    struct list *prev;
};

typedef struct FT_FaceRec_
{
  signed long face_flags;
} FT_FaceRec, *FT_Face;

typedef struct { } GM;

typedef struct { } FMAT2;

typedef struct {
    unsigned int hash;
    LOGFONTW lf;
    int can_use_bitmap;
} FONT_DESC;



typedef struct tagHFONTLIST {
    struct list entry;
    HFONT hfont;
} HFONTLIST;

typedef struct {
    struct list entry;
    void *face;
    GdiFont *font;
} CHILD_FONT;


struct tagGdiFont {
    struct list entry;
    GM **gm;
    struct list hfontlist;
    struct list child_fonts;

    FT_Face ft_face;
    FONT_DESC font_desc;
    long ppem;
};



static struct list gdi_font_list = { &(gdi_font_list), &(gdi_font_list) };




static int get_glyph_index_linked(GdiFont *font, unsigned int c, GdiFont **linked_font, unsigned int *glyph);
static long load_VDMX(GdiFont*, long);

extern int f1(void*,int);

static FT_Face OpenFontFace(GdiFont *font, void *face, long width, long height)
{
   FT_Face ft_face;

   font->ppem = load_VDMX(font, height);
   if(font->ppem == 0)
       font->ppem = f1(ft_face, height);
   return ft_face;
}


static GdiFont *alloc_font(void)
{
    GdiFont *ret = HeapAlloc(0, 0x00000008, sizeof(*ret));
    ret->gm = HeapAlloc(0, 0x00000008, sizeof(GM*));
    return ret;
}


static long load_VDMX(GdiFont *font,long height)
{
    unsigned short hdr[3];

    WineEngGetFontData(font, 0x42424242, 0, hdr, 6);
    return 0;
}

static int fontcmp(const GdiFont *font, FONT_DESC *fd)
{
    if(font->font_desc.hash != fd->hash) return 1;
    if(memcmp(&font->font_desc.lf, &fd->lf, __builtin_offsetof (LOGFONTW, lfFaceName))) return 1;
    if(!font->font_desc.can_use_bitmap != !fd->can_use_bitmap) return 1;
    return strcmpiW(font->font_desc.lf.lfFaceName, fd->lf.lfFaceName);
}

static GdiFont *find_in_cache(HFONT hfont, const LOGFONTW *plf, const FMAT2 *pmat, int can_use_bitmap)
{
    GdiFont *ret;
    FONT_DESC fd;
    HFONTLIST *hflist;
    struct list *font_elem_ptr, *hfontlist_elem_ptr;

    fd.lf = *plf;
    fd.can_use_bitmap = can_use_bitmap;


    for ((font_elem_ptr) = (&gdi_font_list)->next; (font_elem_ptr) != (&gdi_font_list); (font_elem_ptr) = (font_elem_ptr)->next) {
        ret = ((struct tagGdiFont *)((char *)(font_elem_ptr) - (unsigned long)(&((struct tagGdiFont *)0)->entry)));
        if(!fontcmp(ret, &fd)) {
            if(!can_use_bitmap && !( ret->ft_face->face_flags & ( 1L << 0 ) )) continue;
            for ((hfontlist_elem_ptr) = (&ret->hfontlist)->next; (hfontlist_elem_ptr) != (&ret->hfontlist); (hfontlist_elem_ptr) = (hfontlist_elem_ptr)->next) {
                hflist = ((struct tagHFONTLIST *)((char *)(hfontlist_elem_ptr) - (unsigned long)(&((struct tagHFONTLIST *)0)->entry)));
                if(hflist->hfont == hfont)
                    return ret;
            }
            hflist = HeapAlloc(0, 0, sizeof(*hflist));
            hflist->hfont = hfont;
            return ret;
        }
    }

    while(font_elem_ptr) {
        ret = ((struct tagGdiFont *)((char *)(font_elem_ptr) - (unsigned long)(&((struct tagGdiFont *)0)->entry)));
        if(!fontcmp(ret, &fd)) {
            if(!can_use_bitmap && !( ret->ft_face->face_flags & ( 1L << 0 ) )) continue;
            hflist = HeapAlloc(0, 0, sizeof(*hflist));
            hflist->hfont = hfont;
            return ret;
        }
    }
    return ((void *)0);
}




GdiFont *WineEngCreateFontInstance(DC *dc, HFONT hfont)
{
    GdiFont *ret;
    int can_use_bitmap;
    LOGFONTW lf;
    FMAT2 dcmat;

    if((ret = find_in_cache(hfont, &lf, &dcmat, can_use_bitmap)) != ((void *)0))
        return ret;
    return alloc_font();
}

extern unsigned int f(void*,unsigned int g);

static unsigned int get_glyph_index(void*font, unsigned int glyph)
{
    return f(font, glyph);
}

unsigned int WineEngGetGlyphOutline(GdiFont *incoming_font, unsigned int glyph, unsigned int format,
        void* lpgm, unsigned int buflen, void* buf,
        const void* lpmat)
{
    unsigned int glyph_index;

    get_glyph_index_linked(incoming_font, glyph, &incoming_font, &glyph_index);
    return 0;
}

static int load_child_font(GdiFont *font, CHILD_FONT *child)
{
    child->font = alloc_font();
    child->font->ft_face = OpenFontFace(child->font, child->face, 0, -font->ppem);
    if(!child->font->ft_face)
        return 0;
    return 1;
}

static int get_glyph_index_linked(GdiFont *font, unsigned int c, GdiFont **linked_font, unsigned int *glyph)
{
    unsigned int g;
    CHILD_FONT *child_font;

    for ((child_font) = ((CHILD_FONT *)((char *)((&font->child_fonts)->next) - (unsigned long)(&((CHILD_FONT *)0)->entry))); &(child_font)->entry != (&font->child_fonts); (child_font) = ((CHILD_FONT *)((char *)((child_font)->entry.next) - (unsigned long)(&((CHILD_FONT *)0)->entry))))
    {
        if(!load_child_font(font, child_font))
            continue;

        g = get_glyph_index(child_font->font, c);
        if(g) {
            *glyph = g;
            *linked_font = child_font->font;
            return 1;
        }
    }
    return 0;
}

unsigned int WineEngGetFontData(GdiFont *font, unsigned int table, unsigned int offset, void* buf,
    unsigned int cbData)
{
    unsigned long len;
    load_sfnt_table(font->ft_face, table, offset, buf, &len);
    return len;
}

int WineEngGetLinkedHFont(DC *dc, unsigned short c, HFONT *new_hfont, unsigned int *glyph) {
    return get_glyph_index_linked(0, 0, 0, 0);
}

