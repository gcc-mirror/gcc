typedef struct {
    int dummy;
    int width, height;           
} XWindowAttributes;

typedef struct {
    short x, y;
} XPoint;
    
extern unsigned int ya_random (void);
extern int XDrawPoints(XPoint*, int);

static int iterations, offset;
static int  xsym, ysym;

static void
hurm (void)
{
  XWindowAttributes xgwa;
  int xlim, ylim, x, y, i;
  XPoint points [4];


  for (i = 0; i < iterations; i++)
    {
      int j = 0;
      j++;
      if (xsym)
        {
          points [j].x = xlim - x;
          j++;
        }
      points [j].x = x;
      j++;
    }
}

