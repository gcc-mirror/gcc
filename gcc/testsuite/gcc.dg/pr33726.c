/* { dg-do compile } */

/* We used to ICE here with type-checking enabled.  */

typedef unsigned int U032;
typedef volatile struct X {
     U032 Monochrome[1];
     struct {
          U032 WidthHeight;
     } UnclippedRectangle[1];
} RivaBitmap;
void writel(void *);
void rivafb_fillrect(RivaBitmap *bm)
{
  writel((void *)&bm->UnclippedRectangle[0].WidthHeight);
}

