// Origin: abbott@dima.unige.it
// PR c/5120

extern void abort (void);
extern void * malloc (__SIZE_TYPE__);
extern void * calloc (__SIZE_TYPE__, __SIZE_TYPE__);

typedef unsigned int FFelem;

FFelem FFmul(const FFelem x, const FFelem y)
{
  return x;
}


struct DUPFFstruct
{
  int maxdeg;
  int deg;
  FFelem *coeffs;
};

typedef struct DUPFFstruct *DUPFF;


int DUPFFdeg(const DUPFF f)
{
  return f->deg;
}


DUPFF DUPFFnew(const int maxdeg)
{
  DUPFF ans = (DUPFF)malloc(sizeof(struct DUPFFstruct));
  ans->coeffs = 0;
  if (maxdeg >= 0) ans->coeffs = (FFelem*)calloc(maxdeg+1,sizeof(FFelem));
  ans->maxdeg = maxdeg;
  ans->deg = -1;
  return ans;
}

void DUPFFfree(DUPFF x)
{
}

void DUPFFswap(DUPFF x, DUPFF y)
{
}


DUPFF DUPFFcopy(const DUPFF x)
{
  return x;
}


void DUPFFshift_add(DUPFF f, const DUPFF g, int deg, const FFelem coeff)
{
}


DUPFF DUPFFexgcd(DUPFF *fcofac, DUPFF *gcofac, const DUPFF f, const DUPFF g)
{
  DUPFF u, v, uf, ug, vf, vg;
  FFelem q, lcu, lcvrecip, p;
  int df, dg, du, dv;

  __builtin_printf("DUPFFexgcd called on degrees %d and %d\n", DUPFFdeg(f), DUPFFdeg(g));
  if (DUPFFdeg(f) < DUPFFdeg(g)) return DUPFFexgcd(gcofac, fcofac, g, f);  /*** BUG IN THIS LINE ***/
  if (DUPFFdeg(f) != 2 || DUPFFdeg(g) != 1) abort();
  if (f->coeffs[0] == 0) return f;
  /****** NEVER REACH HERE IN THE EXAMPLE ******/
  p = 2;

  df = DUPFFdeg(f);  if (df < 0) df = 0; /* both inputs are zero */
  dg = DUPFFdeg(g);  if (dg < 0) dg = 0; /* one input is zero */
  u = DUPFFcopy(f);
  v = DUPFFcopy(g);

  uf = DUPFFnew(dg); uf->coeffs[0] = 1; uf->deg = 0;
  ug = DUPFFnew(df);
  vf = DUPFFnew(dg);
  vg = DUPFFnew(df); vg->coeffs[0] = 1; vg->deg = 0;

  while (DUPFFdeg(v) > 0)
  {
    dv = DUPFFdeg(v);
    lcvrecip = FFmul(1, v->coeffs[dv]);
    while (DUPFFdeg(u) >= dv)
    {
      du = DUPFFdeg(u);
      lcu = u->coeffs[du];
      q = FFmul(lcu, lcvrecip);
      DUPFFshift_add(u, v, du-dv, p-q);
      DUPFFshift_add(uf, vf, du-dv, p-q);
      DUPFFshift_add(ug, vg, du-dv, p-q);
    }
    DUPFFswap(u, v);
    DUPFFswap(uf, vf);
    DUPFFswap(ug, vg);
  }
  if (DUPFFdeg(v) == 0)
  {
    DUPFFswap(u, v);
    DUPFFswap(uf, vf);
    DUPFFswap(ug, vg);
  }
  DUPFFfree(vf);
  DUPFFfree(vg);
  DUPFFfree(v);
  *fcofac = uf;
  *gcofac = ug;
  return u;
}



int main()
{
  DUPFF f, g, cf, cg, h;
  f = DUPFFnew(1); f->coeffs[1] = 1; f->deg = 1;
  g = DUPFFnew(2); g->coeffs[2] = 1; g->deg = 2;

  __builtin_printf("calling DUPFFexgcd on degrees %d and %d\n", DUPFFdeg(f), DUPFFdeg(g)) ;
  h = DUPFFexgcd(&cf, &cg, f, g);
  return 0;
}
