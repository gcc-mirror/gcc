typedef struct _RunlengthPacket
{
  unsigned short
    red,
    green,
    blue,
    length;
  unsigned short
    index;
} RunlengthPacket;
typedef struct _Image
{
  int
    status,
    temporary;
  char
    filename[1664 ];
  long int
    filesize;
  int
    pipe;
  char
    magick[1664 ],
    *comments,
    *label,
    *text;
  unsigned int
    matte;
  unsigned int
    columns,
    rows,
    depth;
  unsigned int
    scene,
    number_scenes;
  char
    *montage,
    *directory;
  unsigned int
    colors;
  double
    gamma;
  float
    x_resolution,
    y_resolution;
  unsigned int
    mean_error_per_pixel;
  double
    normalized_mean_error,
    normalized_maximum_error;
  unsigned long
    total_colors;
  char
    *signature;
  unsigned int
    packets,
    runlength,
    packet_size;
  unsigned char
    *packed_pixels;
  long int
    magick_time;
  char
    magick_filename[1664 ];
  unsigned int
    magick_columns,
    magick_rows;
  char
    *geometry,
    *page;
  unsigned int
    dispose,
    delay,
    iterations;
  unsigned int
    orphan;
  struct _Image
    *previous,
    *list,
    *next;
} Image;
  Image *MinifyImage(Image *image)
{
  Image
    *minified_image;
  register RunlengthPacket
    *q,
    *s,
    *s0,
    *s1,
    *s2,
    *s3;
  register unsigned int
    x;
  unsigned int
    blue,
    green,
    red;
  unsigned long
    total_matte,
    total_blue,
    total_green,
    total_red;
  unsigned short
    index;
    for (x=0; x < (image->columns-1); x+=2)
    {
      total_red=0;
      total_green=0;
      total_blue=0;
      total_matte=0;
      s=s0;
      total_red+=( 3 )*(s->red); total_green+=( 3 )*(s->green); total_blue+=( 3 )*(s->blue); total_matte+=( 3 )*(s->index); s++; ; total_red+=( 7 )*(s->red); total_green+=( 7 )*(s->green); total_blue+=( 7 )*(s->blue); total_matte+=( 7 )*(s->index); s++; ;  total_red+=( 7 )*(s->red); total_green+=( 7 )*(s->green); total_blue+=( 7 )*(s->blue); total_matte+=( 7 )*(s->index); s++; ;  total_red+=( 3 )*(s->red); total_green+=( 3 )*(s->green); total_blue+=( 3 )*(s->blue); total_matte+=( 3 )*(s->index); s++; ;
      s=s1;
      total_red+=( 7 )*(s->red); total_green+=( 7 )*(s->green); total_blue+=( 7 )*(s->blue); total_matte+=( 7 )*(s->index); s++; ; total_red+=( 15 )*(s->red); total_green+=( 15 )*(s->green); total_blue+=( 15 )*(s->blue); total_matte+=( 15 )*(s->index); s++; ; total_red+=( 15 )*(s->red); total_green+=( 15 )*(s->green); total_blue+=( 15 )*(s->blue); total_matte+=( 15 )*(s->index); s++; ; total_red+=( 7 )*(s->red); total_green+=( 7 )*(s->green); total_blue+=( 7 )*(s->blue); total_matte+=( 7 )*(s->index); s++; ;
      s=s2;
      total_red+=( 7 )*(s->red); total_green+=( 7 )*(s->green); total_blue+=( 7 )*(s->blue); total_matte+=( 7 )*(s->index); s++; ; total_red+=( 15 )*(s->red); total_green+=( 15 )*(s->green); total_blue+=( 15 )*(s->blue); total_matte+=( 15 )*(s->index); s++; ; total_red+=( 15 )*(s->red); total_green+=( 15 )*(s->green); total_blue+=( 15 )*(s->blue); total_matte+=( 15 )*(s->index); s++; ; total_red+=( 7 )*(s->red); total_green+=( 7 )*(s->green); total_blue+=( 7 )*(s->blue); total_matte+=( 7 )*(s->index); s++; ;
      s=s3;
      total_red+=( 3 )*(s->red); total_green+=( 3 )*(s->green); total_blue+=( 3 )*(s->blue); total_matte+=( 3 )*(s->index); s++; ; total_red+=( 7 )*(s->red); total_green+=( 7 )*(s->green); total_blue+=( 7 )*(s->blue); total_matte+=( 7 )*(s->index); s++; ;  total_red+=( 7 )*(s->red); total_green+=( 7 )*(s->green); total_blue+=( 7 )*(s->blue); total_matte+=( 7 )*(s->index); s++; ;  total_red+=( 3 )*(s->red); total_green+=( 3 )*(s->green); total_blue+=( 3 )*(s->blue); total_matte+=( 3 )*(s->index); s++; ;
      red=(unsigned short) ((total_red+63) >> 7);
      green=(unsigned short) ((total_green+63) >> 7);
      blue=(unsigned short) ((total_blue+63) >> 7);
      index=(unsigned short) ((total_matte+63) >> 7);
      if ((red == q->red) && (green == q->green) && (blue == q->blue) &&
          (index == q->index) && ((int) q->length < 65535L ))
        q->length++;
    }
  return(minified_image);
}
