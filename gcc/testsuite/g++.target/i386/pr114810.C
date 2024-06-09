// PR target/114810
// { dg-do compile { target { { { *-*-linux* } && ia32 } && c++17 } } }
// { dg-options "-mstackrealign -O2 -mbmi -fno-exceptions -fno-plt -march=x86-64 -w" }
// { dg-additional-options "-fpie" { target pie } }

enum E1 { a, dp, b, jm, c, dq, d, mj, e, dr, f, jn, h, dt, j, nt, l, du, m, jo, n, dv, o, mk, p, dw, q, jp, s, dx, t, ol, u, dy, v, jq, w };
enum dz { x, ml, y };
struct ea { short g; } z, jr;
long long aa;
struct eb { ea ab; ea dp[]; };
enum ac { };
typedef enum { } nu;
struct ad { ac k; };
unsigned ec (long);
struct ae;
int js (ae);
unsigned af ();
struct ed;
template < int ag > struct ee { using ah = ed[ag]; };
template < int ag > struct array { typename ee < ag >::ah ai; ed & operator[] (int aj) { return ai[aj]; } };
struct { void dp (...); } ak;
void ef (int);
template < typename al > struct jt { al & operator[] (short); };
struct am { void operator= (bool); };
struct an { am operator[] (unsigned); };
template < typename, unsigned, unsigned >using eg = an;
struct ao;
struct ae { ae (ao *); };
struct mm { mm (); mm (int); };
enum ap { };
enum eh { };
bool aq, ju, ar, ei, nv, as, ej, at;
struct jv
{
  jv (eh au):dp (au) {}
  jv ();
  operator eh ();
  unsigned av ()
  {
    aq = dp & 7;
    return dp * (aq ? : 4);
  }
  unsigned ek ()
  {
    int aw;
    bool mn = dp & 7;
    aw = dp * (mn ? : 4);
    return aw + 3 >> 2;
  }
  eh dp;
} ax, el, ay, jw, az, em, ba, om;
struct ed
{
  ed ():bb (), dp () {}
  int bc () { return bb; }
  jv en () { return (eh) dp; }
  unsigned ek ()
  {
    jv bd;
    bd = (eh) dp;
    return bd.ek ();
  }
  ap jx ();
  unsigned bb:24;
  int dp:8;
};
struct be { short dp = 0; } bf, eo;
struct bg
{
  bg ();
  bg (ed r)
  {
    dp.bh = r;
    if (r.bc ())
      mo = true;
    else
      bi = true;
  }
  static bg ep (int);
  bg (be);
  struct { ed bh; } dp;
  union { char mo:1; char bi:1; short bj = 0; };
} jy, bk, eq, bl, mp, bm, er;
struct bn
{
  explicit bn (ed bo):bh (bo) {}
  ed dp ();
  ed bh;
  be es;
  char bj = 0;
};
struct bp
{
  eg < int, 6, 4 > dp;
};
jt < bg > bq;
jt < bn > definitions;
struct ao
{
  bp & br ();
};
enum jz:short;
template < typename > using bs = ae;
ao *et ();
short bt, nw;
struct bu
{
  int dp;
};
dz bv;
unsigned eu;
struct bw
{
  ac k;
  unsigned dp;
} *bx;
bool ka ();
struct by
{
  bool dp;
};
typedef enum
{ bz, ev } ca;
typedef enum
{
  mq, cb, ew, cc, kb, cd, ex, ce
} on;
typedef struct cf
{
  on jx;
  char dp;
  char cg;
} kc;
struct ch
{
  kc *dp;
};
typedef enum
{
  ci, ey, cj, mr, ck, ez, cl, kd, cm, fa, cn, nx, co, fb, cp, ke, cq, fc,
  cr, ms, cs, fd, ct, kf, cu, fe, cv, os, cw, ff, cx, kg, cy, fg, cz, mt, da
} fh;
typedef struct { cf db; fh dp; kc dc; ch kh[]; } dd;
nu fi ();
typedef enum
{ de, ny } fj;
typedef struct { fj jx; } df;
typedef struct { by dp; } dg;
dg *ki;
struct dh
{
  ad *dp;
  eb *args;
  bw *di;
  bu *block;
  struct
  {
    struct
    {
      bool fk;
    } dj;
    bool dp;
    bool dk;
  } mu;
  ed dl[84];
};
ed fl (dh *, ea);
enum dm
{ };
dm kj ();
enum
{ dn };
struct fm
{
  operator ed ();
  operator bg ();
  bn dc ();
  ao *operator-> ();
};
struct oo
{
  oo (ed);
  oo (bg);
  oo (fm);
};
struct fn
{
  enum dp
  {
    kk, fo, mv, fp, kl, fq
  };
  jv lm;
    fn (bw *, bu *);
  fn fr ();
  ed bo (jv);
  bn dc ();
  bn dc (be);
  ed km (oo);
  fm copy (bn, oo);
  fm fs ();
  fm nz (bn, oo, oo, oo);
  void ft (oo);
  void ft (E1, bn, oo);
  fm ft (E1, bn, oo, oo);
  void ft (E1, bn, oo, oo, oo);
  void ft (E1, bn, bn, oo);
  void kn (E1, bn, oo);
  fm kn (dp, bn, oo);
  fm kn (dp, bn, bn, oo);
  fm fu (E1, bn, oo, oo);
  fm fu (E1, bn, bn, oo, oo);
  fm fu (dp, bn, bn, oo, oo);
  fm mw (E1, bn, int);
  fm fv (E1, bn, oo, oo);
  fm fv (dp, bn, oo, oo);
  void dp (E1, bn, mm, bool = false, bool = false, bool = false);
  void dp (E1, bn, oo, oo, mm = false, bool = false, bool = false);
  fm ds (E1, bn, oo, short, char = 0, bool = false);
  fm ds (E1, bn, oo, oo, short = 0, char = 0, bool = false);
  fm ko (E1, bn, oo, oo);
  fm ko (E1, bn, oo, oo, oo);
  fm fw (E1, bn, oo, oo);
  fm dp (E1, bn, oo, oo, oo);
  fm oy (E1, bn, oo, short, char, char, bool, bool);
  fm fx (E1, bn, oo, oo, oo);
};
int kp, fy, mx, fz, kq, ga, oa, gb;
int *kr, *gc, *my;
long gd;
ca ks;
namespace
{
  void ge (dh *, char *, unsigned, cf *, char *);
  ed kt (dh *, kc *);
  ed gf (dh *, ed, bg = bg (), bg = bg ());
  void mz (bool);
  ed gg (dh *, fn, ed, ed);
  ed ku (dh *, fn, ed, unsigned, bool);
  ed gh (dh *, ed);
  void op (dh *, ed, int, ed);
  ed op (dh *, ed, int, jv);
  void gi (dh *, ed, unsigned);
  void kv (dh *, ed, bg, ed);
  ed gj (dh *, ed, ed = ed ());
  void na (dh *, int *);
  void gk (dh *, ed, ed);
  ed kw (dh *);
  bool gl (dh *);
  void ob (dh *, dd *);
  void gm (dh * ctx, dd * db)
  {
    fn bld (ctx->di, ctx->block);
    ed dst = kt (ctx, &db->dc), dp = kt (ctx, db->kh[0].dp), kx, gn;
    unsigned count = db->dc.dp;
    if (db->dc.cg)
      count *= 2;
    if (kr && db->dc.cg)
      {
	long dp = count == 4 ? 0 : (1ull << (count & 3)) - 1 << mx;
	if ((aa | dp) == aa && mx + count)
	  {
	    array < 16 > elems;
	    bs < ao > gn
	    {
	    et ()};
	    unsigned g = ec (aa & mx ? : 1 << mx - 1);
	    for (unsigned i; count; ++i)
	      {
		elems[i] = fl (ctx, ctx->args->dp[g]);
		bq[i] = elems[i];
	      }
	    definitions[0] = bn (dst);
	    int dp = js (gn), ot = dst.bc ();
	    ef (dp);
	    ak.dp (ot, elems);
	  }
      }
    ed go = bld.km (dp);
    if (kp)
      {
	bg dp = bg::ep (kp);
	bn ky = bld.dc (), gp = bld.dc (eo);
	go = bld.fr ().fu (e, ky, gp, dp, go);
      }
    ed nb = fl (ctx, ctx->args->ab);
    bool aligned;
    if (db->dc.cg)
      aligned = kr && fy % 4;
    bool gq = count == 1 || kr && kp + fy % 4 + count;
    if (aligned)
      gn = gq ? bld.bo (ax) : bld.bo (el);
    E1 kz;
    switch (gn.ek ())
      {
      case 1:
	kz = dt;
	break;
      case 2:
	kz = j;
	break;
      case 3:
	gn = bld.bo (ay) = bld.bo (jw);
	ei = true;
      }
    bld.dp (kz, bn (gn), kx, go);
    if (aligned)
      {
	bg dp = kr ? bg::ep (kp + fy % 4) : go;
	kv (ctx, gn, dp, dst);
      }
    if (ei)
      {
	gi (ctx, gn, 4);
	jv dp = dst.ek ()? ax : el;
	ed gr = op (ctx, gn, 2, dp),
	  oc = op (ctx, gn, 0, dp), gs = op (ctx, gn, 1, dp);
	bld.ft (jm, bn (dst), oc, gs, gr);
      }
    gi (ctx, dst, db->dc.dp);
  }
  jz la (ca, unsigned);
  void gt (dh *, dd *, ed);
  bool nc (dh *, dd *);
  void gu (dh *, E1, jz, unsigned, bn, ed);
  void lb (dh *, jz, bn, ed);
  bool gv (dh *, ed &, ed, unsigned, long);
  dh *pb;
  bu *gw;
  dd *lc;
  bool gx;
  jv nd;
  void gy ()
  {
    fn bld (pb->di, gw);
    switch (lc->dp)
      {
      case ms:
	ob (pb, lc);
      case ke:
	gm (pb, lc);
      case fb:
	ob (pb, lc);
      case fc:
	ob (pb, lc);
      case fg:
	ob (pb, lc);
      case cp:
	{
	  if (bv == x || bv == y)
	    {
	      ed dp = kt (pb, &lc->dc), ld = kw (pb);
	      bld.copy (bn (dp), ld);
	    }
	  if (eu <= pb->di->dp)
	    gf (pb, kt (pb, &lc->dc));
	  ed dp = bld.bo (az), bc = gf (pb, dp), gz =
	    fl (pb, jr), od = kt (pb, &lc->dc);
	  if (pb->di->dp)
	    {
	      bn dp = bld.dc (), ha = bld.dc (eo);
	      ed le = fl (pb, jr), hb = bld.fu (dr, dp, ha, jy, le), ne = kt (pb, &lc->dc);
	      bld.ko (jq, bn (ne), hb, bc);
	    }
	  bn hc = bld.dc (), lf = bld.dc (eo);
	  bg hd = bg::ep (0), oq = bg::ep (5);
	  ed hb = bld.fu (f, hc, lf, gz, hd);
	  bld.dp (u, bn (od), hb, oq, bc);
	}
      case cr:
	{
	  ed dp = kt (pb, &lc->dc);
	  gf (pb, dp);
	}
      case mr:
      case ey:
	{
	  ed kh = kt (pb, lc->kh[0].dp), dst = kt (pb, &lc->dc);
	  if (lc)
	    if (lc->kh[0].dp->cg && kh.en ())
	      {
		bn he = bld.dc ();
		bg dp;
		kh = bld.fw (dx, he, dp, kh);
	      }
	  if (lc->kh[0].dp->cg && kh.en ())
	    {
	      bn lg = bld.dc ();
	      bg dp;
	      kh = bld.fw (t, lg, dp, kh);
	    }
	  ge (pb, "", 8, &lc->db, "");
	  bn dc = bld.lm.ek ()? bn (dst) : bld.dc ();
	  if (lc->dp)
	    kh = bld.copy (dc, kh);
	  else
	    {
	      bn hf = bld.dc ();
	      kh = bld.fu (fn::kk, dc, hf, kh, bg (bf));
	    }
	  if (dst.ek () != bld.lm.ek ())
	    {
	      bg dp;
	      bld.ft (jm, bn (dst), kh, dp);
	    }
	  mz (pb);
	}
      case nx:
	{
	  ed nf = kt (pb, lc->kh[0].dp), kh = bld.km (nf), dst = kt (pb, &lc->dc);
	  if (kh.ek ())
	    op (pb, kh, 0, dst);
	  if (dst.ek ())
	    {
	      bg dp;
	      bld.ft (jm, bn (dst), kh, dp);
	    }
	  bld.copy (bn (dst), kh);
	}
      case cy:
      case ff:
	{
	  ed kh = kt (pb, lc->kh[0].dp), delta = kt (pb, lc->kh[1].dp), dst =
	    kt (pb, &lc->dc), bo, hg, hi, dp = bld.bo (az), hh = gf (pb, dp), lh = bld.fs ();
	  if (ka ())
	    gt (pb, lc, kh);
	  {
	    ed hh = kt (pb, lc->kh[1].dp) = kt (pb, &lc->dc) = gh (pb, kh);
	    if (lc || ka ())
	      hh = bld.km (hh);
	    if (nd == ba || kh.en ())
	      bo = bld.bo (az) = gg (pb, bld, hh, kh);
	    if (dst.jx ())
	      bld.dc ();
	    bld.ft (mj, bn (dst), bo);
	    bld.ft (b, bn (dst), bo);
	    if (kh.en ())
	      bo = gg (pb, bld, hh, kh);
	    bld.copy (bn (dst), bo);
	    if (kh.en () == em)
	      {
		hg = bld.bo (az) = bld.bo (az);
		bld.ft (mj, bn (hg), kh);
		gg (pb, bld, hh, hg) = gg (pb, bld, hh, hi);
		bld.ft (jm, bn (dst), hg, hi);
		gi (pb, dst, 2);
	      }
	    ge (pb, "", 4, &lc->db, "");
	    mz (pb);
	  }
	  kt (pb, lc->kh[0].dp);
	  if (ka ())
	    gt (pb, lc, kh);
	  if (fz)
	    bld.copy (bn (dst), kh);
	  delta = bld.km (delta) = gh (pb, kh);
	  if (ka () && gv (pb, bo, kh, fz, gd))
	    {
	      bn dp = bld.dc (), oe = bld.dc ();
	      bg hj = bg::ep (0);
	      ed li =
		bld.fv (jn, dp, delta, hj), hk = ku (pb, bld, kh, oa, true);
	      gj (pb, li) = bld.ko (ol, oe, hk, kh, li);
	    }
	  if (pb->di->k && fz)
	    {
	      bn dp = bld.dc (), ng = bld.dc (eo), hl = bld.dc (), lj = bld.dc (eo);
	      delta = bld.fu (dr, dp, ng, delta, bk);
	      bg hm = bg::ep (2);
	      delta = bld.fu (nt, hl, lj, delta, hm);
	    }
	  bn ou = bld.dc (), hn = bld.dc (), lk = bld.dc (), ho = bld.dc (eo) = bld.dc (), nh = bld.dc (eo) = bld.dc (), hp = bld.dc (eo) = bld.dc (), ll = bld.dc (eo), hq = bld.dc ();
	  bg of = bg::ep (0), hr = bg::ep (64) = bld.fu (q, nh, hr, delta);
	  if (fz)
	    {
	      bn dp = bld.dc (), ni = bld.dc () = bld.dc (), hs = bld.dc (), ln = bld.dc (), ht = bld.dc (), oz = bld.dc (), hu = bld.dc (), lo = bld.dc ();
	      ed hv = bld.fu (du, ni, hg, delta), nj = bld.fu (nt, hs, hg, delta);
	      if (fz)
		{
		  bn dp = bld.dc () = bld.dc (), hw = bld.dc (), lp = bld.dc () = bld.dc (), hx = bld.dc ();
		  bg og = bg::ep (4444), hy = bg::ep (32);
		  ed lotolohi = bld.copy (dp, og), hz = bld.fu (dv, hw, hv, lotolohi) =
		    bld.fu (mk, lp, hv, hz) = bld.fu (q, hx, hy, delta);
		}
	      hg = bld.fu (dv, ln, ht, hv, nj);
	      bg lq = bg::ep (88888888);
	      ed ia = bld.copy (oz, lq);
	      hi = bld.fu (dv, hu, lo, hg, ia);
	    }
	  else
	    {
	      bn dp = bld.dc ();
	      bg nk = bg::ep (4275878552);
	      hi = bld.copy (dp, nk);
	    }
	  ed hz = bld.ft (jm, hn, hg, hi);
	  ed hv = bld.fu (m, lk, ho, hz, delta);
	  ed nj = bld.fu (l, hp, hz, delta);
	  hz = bld.fu (o, ll, hv, nj);
	  hg = bld.bo (ax) = bld.bo (ax);
	  bld.ft (mj, bn (hg), bn (hi), hz);
	  fm ib = bld.dp (w, hq, kh, hg, hi);
	  bp & lr = ib->br (), &ic = ib->br ();
	  lr.dp[0] = true;
	  ic.dp[1] = true;
	  if (pb->di && fz)
	    {
	      bn dp = bld.dc (), ov = bld.dc (), id = bld.dc ();
	      bg ls = bg::ep (2), ie = bg::ep (1);
	      ed nl = bld.ko (dy, dp, ls, lh);
	      bo = bld.ds (a, ov, nl, kh);
	      lh = bld.dp (s, id, ie, lh, hh);
	    }
	  bo = gg (pb, bld, lh, kh);
	  jv ig;
	  bo = op (pb, bo, 0, ig);
	  bld.copy (bn (dst), bo);
	  mz (pb);
	}
      case cq:
	{
	  ed dp = kt (pb, &lc->dc), lt = fl (pb, z);
	  bg ih = bg::ep (8), oh = bg::ep (4);
	  bld.dp (jp, bn (dp), lt, ih, oh);
	}
      case cw:
	{
	  ed kh = kt (pb, lc->kh[0].dp), dst = kt (pb, &lc->dc);
	  if (lc->dc.cg)
	    {
	      bn dp = bld.dc (eo), ii = bld.dc ();
	      fm lu = bld.kn (fn::kl, ii, bg (bf));
	      ed bo = bld.fv (fn::fp, dp, kh, lu);
	      gj (pb, dst);
	    }
	  gk (pb, kh, dst);
	  mz (pb);
	}
      case ci:
	{
	  ed kh = kt (pb, lc->kh[0].dp), dst = kt (pb, &lc->dc);
	  if (kh.jx ())
	    bld.ft (b, bn (dst), kh);
	  bld.copy (bn (dst), kh);
	}
      case mt:
	{
	  ed kh = kt (pb, lc->kh[0].dp), dst = kt (pb, &lc->dc);
	  bn dp = bld.dc (), ij = bld.dc (eo), nm =
	    bld.dc (), ik = bld.dc (eo), lv = bld.dc (eo);
	  ed bo = bld.kn (fn::fo, dp, ij, kh);
	  bld.fu (fn::kk, nm, ik, bo, bg (bf)).dc ().dp ();
	  ed cond = gj (pb, bo);
	  bld.kn (fn::fo, bn (dst), lv, cond);
	  mz (pb);
	}
      case da:
	{
	  ed kh = kt (pb, lc->kh[0].dp), dst = kt (pb, &lc->dc) = gj (pb, kh);
	  gj (pb, dst);
	}
      case os:
	{
	  ed kh = kt (pb, lc->kh[0].dp), dp = kt (pb, &lc->dc);
	  bn il = bld.dc (), pc = bld.dc (eo), im = bld.dc (eo);
	  kh = bld.fu (fn::kk, il, pc, kh, bg (bf));
	  bld.kn (fn::mv, bn (dp), im, kh);
	  mz (pb);
	}
      case cv:
	{
	  ed kh = kt (pb, lc->kh[0].dp), dp = kt (pb, &lc->dc);
	  bn lw = bld.dc (), in = bld.dc (eo), nn = bld.dc (), io = bld.dc (eo), lx = bld.dc (), ip = bld.dc (eo), oi = bld.dc (eo);
	  kh = bld.kn (fn::fo, lw, in, kh);
	  kh = bld.fu (fn::kk, nn, io, kh, bg (bf));
	  kh = bld.kn (fn::mv, lx, ip, kh);
	  bld.kn (fn::fo, bn (dp), oi, kh);
	  mz (pb);
	}
      case cx:
      case cn:
      case cm:
	{
	  ed kh = kt (pb, lc->kh[0].dp), dst = kt (pb, &lc->dc);
	  unsigned dp = lc->dp ? af () : af ();
	  if (ka () == pb->di->dp)
	    if (lc->dp)
	      if (nc (pb, lc))
		if (nc (pb, lc))
		  break;
	  jv iq;
	  kh = op (pb, kh, 0, iq);
	  jz ly = la (ks, lc->kh[0].dp->cg);
	  switch (lc->dp)
	    {
	    case cx:
	    case cn:
	    case cm:
	      break;
	    default:
	      __builtin_unreachable ();
	    }
	  bool ir = ks == bz || ks == ev && dst.jx ();
	  if (ir)
	    lb (pb, ly, bn (dst), kh);
	  gu (pb, dq, ly, dp, bn (dst), kh);
	  mz (pb);
	}
      case fe:
	{
	  ed kh = kt (pb, lc->kh[0].dp), dst = kt (pb, &lc->dc);
	  if (gx)
	    gt (pb, lc, kh);
	  switch (lc->dp)
	    {
	    case kf:
	      kj ();
	    case cu:
	    case ct:
	      nw = kj ();
	    case fe:
	      nw = af ();
	      nv &= ka ();
	    case fd:
	      nw = kj ();
	    }
	  if (lc->dc.cg)
	    {
	      bn dp = bld.dc ();
	      bg lz;
	      kh = bld.fx (ol, dp, lz, eq, kh);
	    }
	  if (lc->dc.cg)
	    kh = gh (pb, kh);
	  if (lc->dc.cg && lc->dp)
	    {
	      bn dp = bld.dc (), is = bld.dc (), no = bld.dc (eo), it = bld.dc (), ma = bld.dc (eo), iu = bld.dc (eo);
	      bg oj = bld.lm.av ()? bg::ep (kq) : bld.ft (jm, dp, bl, mp);
	      kh = bld.fu (fn::kk, is, no, kh, bg (bf));
	      kh = bld.fu (fn::kk, it, ma, oj, kh);
	      bld.kn (fn::mv, bn (dst), iu, kh);
	    }
	  if (lc || lc->dc.cg)
	    {
	      unsigned iv = lc->dc.cg / 8;
	      bn dc = iv || lc->dc.cg ? bld.dc () : bn (dst);
	      if (pb->di->k)
		bld.oy (v, dc, kh, nw, 5, 5, true, nv);
	      bld.ds (dp, dc, kh, 5 | nw);
	      if (iv)
		{
		  bn dp = bld.dc ();
		  ed mb = dc.dp ();
		  bld.ft (mj, bn (dst), dp, mb);
		}
	      if (lc->dc.cg)
		{
		  bg dp;
		  ed iw = dc.dp ();
		  bld.fw (dx, bn (dst), dp, iw);
		}
	    }
	  if (lc->dc.cg)
	    {
	      ed hg, hi = bld.ft (mj, bn (hg), hi, kh);
	      if (pb->di->k)
		{
		  bn dp = bld.dc (), np = bld.dc ();
		  hg = bld.oy (v, dp, hg, nw, 5, 5, true, nv);
		  hi = bld.oy (v, np, hi, nw, 5, 5, true, nv);
		}
	      bn ix = bld.dc (), mc = bld.dc ();
	      hg = bld.ds (dp, ix, hg, nw);
	      hi = bld.ds (dp, mc, hi, 5 | nw);
	      bld.ft (jm, bn (dst), hg, hi);
	      gi (pb, dst, 2);
	    }
	  ge (pb, "", 4, &lc->db, "");
	  mz (pb);
	  kh = kt (pb, lc->kh[0].dp) = kt (pb, &lc->dc);
	  gt (pb, lc, kh);
	  bool dp = ka ();
	  if (lc->dc.cg)
	    kh = gh (pb, kh);
	  if (lc->dc.cg)
	    {
	      bn iy = bld.dc ();
	      bg ow, iz;
	      kh = bld.fx (ol, iy, ow, bm, kh);
	      kh = ku (pb, bld, kh, ga, dp);
	      bld.fw (dx, bn (dst), iz, kh);
	    }
	  if (dst.en ())
	    {
	      ed bo = ku (pb, bld, kh, ga, dp);
	      op (pb, bo, 0, dst);
	    }
	  if (dst.en () == om)
	    {
	      ed bo = ku (pb, bld, kh, ga, dp);
	      op (pb, bo, 0, dst);
	    }
	  if (dst.en () == az)
	    {
	      ed md = ku (pb, bld, kh, ga, dp);
	      bld.copy (bn (dst), md);
	    }
	  if (dst.en () == em)
	    {
	      ed hg = bld.bo (az), hi = bld.bo (az);
	      bld.ft (mj, bn (hg), hi, kh);
	      hg = ku (pb, bld, hg, ga, dp) = ku (pb, bld, hi, ga, dp);
	      bld.ft (jm, bn (dst), hg, hi);
	      gi (pb, dst, 2);
	    }
	  ge (pb, "", 5, &lc->db, "");
	  mz (pb);
	}
	{
	  ed dp = kt (pb, lc->kh[0].dp), kh = gh (pb, dp), ja = kt (pb, lc->kh[1].dp), nq = bld.km (ja), jb = kt (pb, lc->kh[2].dp), lane = bld.km (jb), dst = kt (pb, &lc->dc);
	  if (dst.en ())
	    bld.nz (bn (dst), nq, lane, kh);
	  if (dst.en ())
	    {
	      ed dp = bld.bo (az), jc = bld.bo (az), me = bld.bo (ax), jd = bld.bo (ax);
	      bld.ft (mj, bn (dp), bn (jc), kh);
	      bld.ft (mj, bn (me), bn (jd), nq);
	      bn ok = bld.dc (), je = bld.dc ();
	      ed hg = bld.nz (ok, me, lane, jc);
	      ed hi = bld.nz (je, jd, lane, jc);
	      bld.ft (jm, bn (dst), hg, hi);
	      gi (pb, dst, 2);
	    }
	  ge (pb, "", 8, &lc->db, "");
	}
      case cs:
	{
	  ed kh = kt (pb, lc->kh[0].dp), dp = kt (pb, lc->kh[1].dp), mf = gh (pb, dp), dst = kt (pb, &lc->dc) = op (pb, kh, 0, jv ());
	  gf (pb, dst, kh, mf);
	  mz (pb);
	  kh = kt (pb, lc->kh[0].dp) = kt (pb, &lc->dc);
	  if (kh.en ())
	    bld.copy (bn (dst), kh);
	  if (dst.en () && kh.en ())
	    {
	      ed dp = kt (pb, lc->kh[1].dp), jf = bld.km (dp), nr = kt (pb, lc->kh[2].dp), jg = bld.km (nr);
	      bld.dp (w, bn (dst), kh, jf, jg);
	    }
	  ge (pb, "", 4, &lc->db, "");
	  kt (pb, &lc->dc);
	  bld.ft (d, bn (dst), bg ());
	}
	if (lc->dp)
	  {
	    ed kh = kt (pb, lc->kh[0].dp);
	    bn dp = bld.dc (), mg = bld.dc (eo);
	    er = bld.fu (fn::kk, dp, mg, kh, bg (bf));
	  }
	bld.ft (er);
	if (bt || pb->mu.dj.fk)
	  pb->mu.dk = gw->dp |= ju;
	if (ki->dp.dp)
	  ar = true;
      case cz:
      case ck:
      case cl:
	{
	  bg cond = bg::ep (1);
	  if (lc->dp == cl || lc->dp == cz)
	    {
	      ed kh = kt (pb, lc->kh[0].dp);
	      bn dp = bld.dc (), jh = bld.dc (eo);
	      cond = bld.fu (fn::kk, dp, jh, kh, bg (bf));
	      pb->mu.dp |= ka ();
	    }
	  bld.ft (cond);
	  if (bt || pb->mu.dj.fk)
	    pb->mu.dk = pb->mu.dp |= gl (pb);
	  gw->dp |= ju;
	}
      case fa:
	{
	  ed dp = kt (pb, &lc->dc);
	  bld.kn (fn::kl, bn (dp), bg (bf));
	}
	mz (pb);
      case co:
	{
	  bn pa = bld.dc (), dp = bld.dc (eo);
	  ed flbit = bld.kn (fn::fq, pa, bg (bf)), ji = kt (pb, &lc->dc);
	  bg mh = bg::ep (pb->di->dp - 1);
	  bld.fu (dw, bn (ji), dp, mh, flbit);
	  mz (pb);
	}
      case kd:
	{
	  ed dp = kt (pb, &lc->dc);
	  bld.ft (c, bn (dp), bg (bf));
	}
	mz (pb);
      case kg:
	ed dst = kt (pb, &lc->dc);
	if (fi () && pb->dp->k)
	  {
	    bn jj = bld.dc ();
	    ed clock = bld.mw (h, jj, 9);
	    bg dp;
	    bld.ft (jm, bn (dst), clock, dp);
	  }
	if (fi () && pb->dp->k)
	  {
	    bg ns = bg::ep (dn);
	    bld.kn (p, bn (dst), ns);
	    E1 dp = fi ()? jo : n;
	    bld.dp (dp, bn (dst), 0);
	  }
	gi (pb, dst, 2);
      }
  }
  dh *jk;
  void mi (bool, bool, bool)
  {
    mm ();
    for (df * dp (ka ()? (df *) bx : __null);
	 dp; dp = ka ()? (df *) bx : __null)
      switch (dp->jx)
	{
	case de:
	  for (cf * db (ka ()? (cf *) bx : __null);
	       db; db = ka ()? (cf *) bx : __null)
	    switch (db->jx)
	      {
	      case mq:
		na (jk, gc);
	      case kb:
		na (jk, my);
	      case cc:
		gy ();
	      case ew:
		mm ();
	      case ce:
		mm ();
	      case ex:
		mm ();
	      case cb:
	      case cd:
		mm ();
		ge (jk, "", 6, db, "");
	      }
	  mm ();
	case ny:
	  mm ();
	}
  }
}
void
jl ()
{
  dh ctx;
  if (gb)
    mi (as, ej, at);
}
