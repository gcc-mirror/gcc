/* { dg-require-effective-target alloca } */
/* { dg-skip-if "too many arguments in function call" { bpf-*-* } } */
/* { dg-additional-options "-std=gnu89" } */

main ()
{
  char *a;
  foo (alloca (10000));
  foo (alloca (100000));
  foo (alloca ((int) &main));
}

many_par (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
{
  char *x;
  int aa, ba, ca, da, ea, fa, ga, ha, ia, ja, ka, la, ma, na, oa, pa;

  aa = bar ();
  ba = bar ();
  ca = bar ();
  da = bar ();
  ea = bar ();
  fa = bar ();
  ga = bar ();
  ha = bar ();
  ia = bar ();
  ja = bar ();
  ka = bar ();
  la = bar ();
  ma = bar ();
  na = bar ();
  oa = bar ();
  pa = bar ();
  foobar (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, aa, ba, ca,
	  da, ea, fa, ga, ha, ia, ja, ka, la, ma, na, oa, pa);

}

foobar (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, aa, ba, ca,
	da, ea, fa, ga, ha, ia, ja, ka, la, ma, na, oa, pa)
{
  int ab, bb, cb, db, eb, fb, gb, hb, ib, jb, kb, lb, mb, nb, ob, pb;
  int qb, rb, sb, tb, ub, vb, xb, yb;

  ab = bar ();
  bb = bar ();
  cb = bar ();
  db = bar ();
  eb = bar ();
  fb = bar ();
  gb = bar ();
  hb = bar ();
  ib = bar ();
  jb = bar ();
  kb = bar ();
  lb = bar ();
  mb = bar ();
  nb = bar ();
  ob = bar ();
  pb = bar ();
  qb = bar ();
  rb = bar ();
  sb = bar ();
  tb = bar ();
  ub = bar ();
  vb = bar ();
  xb = bar ();
  yb = bar ();

  boofar (a);
  boofar (b);
  boofar (c);
  boofar (d);
  boofar (e);
  boofar (f);
  boofar (g);
  boofar (h);
  boofar (i);
  boofar (j);
  boofar (k);
  boofar (l);
  boofar (m);
  boofar (n);
  boofar (o);
  boofar (p);
  boofar (aa);
  boofar (ba);
  boofar (ca);
  boofar (da);
  boofar (ea);
  boofar (fa);
  boofar (ga);
  boofar (ha);
  boofar (ia);
  boofar (ja);
  boofar (ka);
  boofar (la);
  boofar (ma);
  boofar (na);
  boofar (oa);
  boofar (pa);

  boofar (ab);
  boofar (bb);
  boofar (cb);
  boofar (db);
  boofar (eb);
  boofar (fb);
  boofar (gb);
  boofar (hb);
  boofar (ib);
  boofar (jb);
  boofar (kb);
  boofar (lb);
  boofar (mb);
  boofar (nb);
  boofar (ob);
  boofar (pb);

  boofar (a);
  boofar (b);
  boofar (c);
  boofar (d);
  boofar (e);
  boofar (f);
  boofar (g);
  boofar (h);
  boofar (i);
  boofar (j);
  boofar (k);
  boofar (l);
  boofar (m);
  boofar (n);
  boofar (o);
  boofar (p);
  boofar (aa);
  boofar (ba);
  boofar (ca);
  boofar (da);
  boofar (ea);
  boofar (fa);
  boofar (ga);
  boofar (ha);
  boofar (ia);
  boofar (ja);
  boofar (ka);
  boofar (la);
  boofar (ma);
  boofar (na);
  boofar (oa);
  boofar (pa);

  boofar (ab);
  boofar (bb);
  boofar (cb);
  boofar (db);
  boofar (eb);
  boofar (fb);
  boofar (gb);
  boofar (hb);
  boofar (ib);
  boofar (jb);
  boofar (kb);
  boofar (lb);
  boofar (mb);
  boofar (nb);
  boofar (ob);
  boofar (pb);

}

test_exit_ignore_stack ()
{
  foobar (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
}
