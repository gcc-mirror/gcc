/* ICE: call insn does not satisfy its constraints, MMIX port.
   Origin: ghostscript-6.52, reduction from hp@bitrange.com.  */
struct s0
{
  void (*init_color)(void *, void *);
};
struct s1
{
  void (*map_cmyk)(short, void *, void **, void *);
  void (*map_rgb_alpha)(short, void *, void **, void *);
};
struct s5
{
  long fill1; int fill2;
  long fill3; unsigned int fill4, fill5;
};
struct s2
{
  struct s5 x, y;
};
struct s3
{
  long dev_color;
  unsigned int key;
};
struct s4
{
  unsigned char spp;
  int alpha;
  struct mc_
  {
    unsigned int values[14];
    unsigned int mask, test;
    int exact;
  } mask_color;
  void **pis;
  struct s0 *pcs;
  struct dd_
  {
    struct s2 row[2];
    struct s2 pixel0;
  } dda;
  struct s3 clues[256];
};
extern struct s1 *get_cmap_procs (void **, void *);
int image_render_color (struct s4 *, unsigned char *, int, void *);
int
image_render_color (struct s4 *penum, unsigned char *buffer,
		    int data_x, void *dev) 
{
  struct s3 *clues = penum->clues;
  void **pis = penum->pis;
  struct s2 pnext;
  struct s0 *pcs = penum->pcs;
  struct s1 *cmap_procs = get_cmap_procs(pis, dev);
  void (*map_4)(short, void *, void **, void *) =
    (penum->alpha ? cmap_procs->map_rgb_alpha : cmap_procs->map_cmyk);
  unsigned int mask = penum->mask_color.mask;
  unsigned int test = penum->mask_color.test;
  struct s3 *pic_next = &clues[1];
  int spp = penum->spp;
  unsigned char *psrc = buffer + data_x * spp;
  unsigned char v[6];

  pnext = penum->dda.pixel0;
  __builtin_memset (&v, 0, sizeof(v));
  (*(pcs)->init_color) (0, 0);

  if (spp == 4)
    {
      v[0] = psrc[0];
      v[1] = psrc[1];
      if ((buffer[0] & mask) == test && penum->mask_color.exact)
	pic_next->dev_color = 0;
      (*map_4)(v[0], &pic_next->dev_color, pis, dev);
    }
  return 0;
}
