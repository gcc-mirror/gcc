extern int foobar1 ();

typedef struct
  {
    unsigned long colormap;
    unsigned long red_max;
    unsigned long red_mult;
    unsigned long green_max;
    unsigned long green_mult;
    unsigned long blue_max;
    unsigned long blue_mult;
    unsigned long base_pixel;
    unsigned long visualid;
    unsigned long killid;
  }
frotz;

int
foobar (stdcmap, count)
     frotz **stdcmap;
     int *count;
{
  register int i;
  frotz *data = ((void *) 0);

  unsigned long nitems;
  int ncmaps;
  int old_style = 0;
  unsigned long def_visual = 0L;
  frotz *cmaps;


  if ( foobar1 (&data) != 0)
    return 0;
  if (nitems < 10)
    {
      ncmaps = 1;
      if (nitems < 9)
	{
	}
    }
  else
    ncmaps = (nitems / 10);

  {
    register frotz *map;
    register frotz *prop;

    for (i = ncmaps, map = cmaps, prop = data; i > 0; i--, map++, prop++)
      {
	map->colormap = prop->colormap;
	map->red_max = prop->red_max;
	map->red_mult = prop->red_mult;
	map->green_max = prop->green_max;
	map->green_mult = prop->green_mult;
	map->blue_max = prop->blue_max;
	map->blue_mult = prop->blue_mult;
	map->base_pixel = prop->base_pixel;
	map->visualid = (def_visual ? def_visual : prop->visualid);
	map->killid = (old_style ? 0L : prop->killid);
      }
  }
  *stdcmap = cmaps;
  *count = ncmaps;
}
