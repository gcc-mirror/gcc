/* { dg-do compile } */
/* { dg-options "-O3 -mavx2" } */

typedef unsigned int U32;

struct sv {
  void* sv_any;
  U32 sv_refcnt;
  U32 sv_flags;
};
typedef struct sv SV;

struct xrv {
  SV * xrv_rv;
};
typedef struct xrv XRV;

extern XRV * PL_xrv_root;

void
more_xrv (void)
{
  register XRV* xrv;
  register XRV* xrvend;
  xrv = PL_xrv_root;
  xrvend = &xrv[200 / sizeof (XRV) - 1];
  while (xrv < xrvend)
  {
    xrv->xrv_rv = (SV*)(xrv + 1);
    xrv++;
  }
  xrv->xrv_rv = 0;
}
