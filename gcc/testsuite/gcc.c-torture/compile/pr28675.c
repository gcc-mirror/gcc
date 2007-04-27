struct fb_cmap {
 unsigned int start;
 unsigned int len;
 unsigned short *red;
 unsigned short *green;
 unsigned short *blue;
 unsigned short *transp;
};

typedef struct {
    int r;
    int g;
    int b;
    int a;
} rgba_t;

static unsigned int cmap_len;

extern unsigned int red_len, green_len, blue_len, alpha_len;
extern struct fb_cmap fb_cmap;
extern rgba_t *clut;
extern int fb_set_cmap(void);

void directcolor_update_cmap(void)
{
    unsigned int i;

    for (i = 0; i < cmap_len; i++) {
      if (i < red_len)
	fb_cmap.red[i] = clut[i].r;
      if (i < green_len)
	fb_cmap.green[i] = clut[i].g;
      if (i < blue_len)
	fb_cmap.blue[i] = clut[i].b;
      if (fb_cmap.transp && i < alpha_len)
	fb_cmap.transp[i] = clut[i].a;
    }
}
