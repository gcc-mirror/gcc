/* { dg-do compile } */

struct gl_visual {
    float AlphaScale;
};
struct gl_context {
    struct gl_visual *Visual;
};
void quickdraw_rgb( struct gl_context * ctx,
		    int width, int height)
{
  int i, j;
  unsigned char alpha[1600];
  for (j=0; j<width; j++)
    alpha[j] = (int) ctx->Visual->AlphaScale; 
  for (i=0; i<height; i++)
    foo( alpha);
}

