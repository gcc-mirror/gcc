/* { dg-do compile } */
/* { dg-options "-O2 -floop-nest-optimize -fdump-tree-graphite" } */

int qc;

int
ec (int lh[][2])
{
  const int jv = 3;
  int zf, hp, c5 = 0, m3 = 1;

  for (zf = 0; zf < jv; ++zf)
    for (hp = 0; hp < jv; ++hp)
      {
	short int bm = 0;

	for (qc = 0; qc < jv; ++qc)
	  --bm;
	if (bm != 0)
	  --c5;
	lh[0][0] = 0;
	m3 *= jv;
      }

  return c5 + m3;
}

/* { dg-final { scan-tree-dump "isl AST to Gimple succeeded" "graphite" } } */
