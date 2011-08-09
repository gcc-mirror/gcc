/* { dg-do run } */
/* { dg-options "-O3" } */

#define PACKED __attribute__(( packed ))

struct PACKED aostk_point_u8 {
  unsigned char x;
  unsigned char y;
};

struct PACKED aostk_size_u8 {
  unsigned char width;
  unsigned char height;
};

struct PACKED aostk_glyph {
	unsigned short i;
	struct aostk_size_u8 size;
	char top;
	struct aostk_point_u8 advance;
	unsigned char pitch;
	unsigned char* data;
	char left;
};


struct PACKED aostk_font {
	unsigned short numglyphs;
	unsigned char height;
	struct aostk_glyph* glyphs;
};

struct aostk_font glob_font;

static struct aostk_glyph* aostk_get_glyph(struct aostk_font* f, unsigned int c) {
	return f->glyphs;
}

int aostk_font_strwidth(struct aostk_font* font, const char* str) {
	struct aostk_glyph* g = aostk_get_glyph(font, 0);
	return (g != 0);
}

struct aostk_font*
__attribute__ ((noinline, noclone))
get_some_font (void)
{
  return &glob_font;
}

int main (int argc, char *argv[])
{
  return (int) aostk_font_strwidth (get_some_font (), "sth");
  
}
