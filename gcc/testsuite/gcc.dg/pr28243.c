/* PR rtl-optimization/28243 */
/* Reported by Mike Frysinger <vapier@gentoo.org> */

/* { dg-do compile } */
/* { dg-options "-O2 -ftracer -fPIC" } */

struct displayfuncs {
  void (*init) ();
} funcs;

struct gpsdisplay {
  struct displayfuncs *funcs;
};

static void PSMyArc(double cx, double cy, double radx, double rady, double sa,
		    double ta)
{
  double ea;
  double temp;
  ea = sa + ta;
  while (sa < ea) {
    temp = ((sa + 90) / 90) * 90;
    PSDoArc(cx, sa, ea < temp ? ea : temp);
    sa = temp;
  }
}

static void PSDrawElipse()
{
  float cx;
  float cy;
  float radx;
  float rady;
  if (radx != rady)
    PSMyArc(cx, cy, radx, rady, 0, 360);
}

static void PSDrawFillCircle()
{
  PSDrawElipse();
}

static struct displayfuncs psfuncs[] = {
  PSDrawFillCircle
};

void _GPSDraw_CreateDisplay()
{
  struct gpsdisplay *gdisp;
  gdisp->funcs = (void *)&psfuncs;
}
