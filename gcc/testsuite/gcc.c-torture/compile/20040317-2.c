/* { dg-skip-if "too many arguments in function call" { bpf-*-* } } */

typedef struct _ScaleRec *ScaleWidget;
typedef struct
{
  short *x;
  unsigned short *width;
} Table;
typedef struct
{
  Table table;
} ScalePart;
typedef struct _ScaleRec
{
  ScalePart scale;
} ScaleRec;
static int
FindPixel (ScaleWidget sw, short x, short y,
       short * img_x, short * img_y, unsigned long * img_pixel)
{
  if (sw->scale.table.x[(int) *img_x] + 
      (short) sw->scale.table.width[(int) *img_x] < x)
    {
      ++*img_x;
      return FindPixel (sw, x, y, img_x, img_y, img_pixel);
    }
}
