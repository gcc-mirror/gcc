/* { dg-do compile } */
/* { dg-options "-O -fgimple -fdump-tree-forwprop1 -Wno-psabi" } */

typedef float v4sf __attribute__ ((vector_size (16)));
typedef int v4si __attribute__ ((vector_size (16)));
typedef v4si v4sib __attribute__ ((vector_mask));

v4sf __GIMPLE (ssa)
f (v4sf a, v4sf b)
{
  v4sib m;
  v4si s;
  v4sf r;

__BB(2):
  m_3 = a_1(D) >= b_2(D);
  s_4 = m_3 ? _Literal (v4si) { 0, 0, 0, 0 }
	      : _Literal (v4si) { 1, 1, 1, 1 };
  r_5 = (v4sf) s_4;
  return r_5;
}

v4si __GIMPLE (ssa)
g (v4sf a, v4sf b)
{
  v4sib m;
  v4sf s;
  v4si r;

__BB(2):
  m_3 = a_1(D) < b_2(D);
  s_4 = m_3 ? _Literal (v4sf) { 2.5f, 2.5f, 2.5f, 2.5f }
	      : _Literal (v4sf) { 3.75f, 3.75f, 3.75f, 3.75f };
  r_5 = (v4si) s_4;
  return r_5;
}

/* { dg-final { scan-tree-dump-not "= \\(v4s\[fi\]\\)" "forwprop1" } } */
/* { dg-final { scan-tree-dump "<m_3, { 0\\.0, 0\\.0, 0\\.0, 0\\.0 }, { 1\\.0e\\+0, 1\\.0e\\+0, 1\\.0e\\+0, 1\\.0e\\+0 }>" "forwprop1" } } */
/* { dg-final { scan-tree-dump "<m_3, { 2, 2, 2, 2 }, { 3, 3, 3, 3 }>" "forwprop1" } } */
