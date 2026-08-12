/* { dg-do compile } */
/* { dg-options "-O2 --param=vrp-block-limit=1" } */

/* gori_name_helper walks both SSA operands of every statement with no
   memoisation, so a depth-N DAG of two-operand statements costs 2^N.

   Entry point: gori_name_helper, gcc/gimple-range-gori.cc:1683, reached from
   gori_name_on_edge (1729) <- dom_ranger::range_on_edge (gimple-range.cc:858)
   <- fvrp_folder::value_on_edge, once per PHI argument.

   The sibling walk gori_calc_operands (gimple-range-gori.cc:1617) memoises
   with !r.has_range (si.ssaN) and is linear; this one has no cache, no depth
   limit and no in_chain_p test.

   Needs the fast-VRP path, selected automatically when the function has more
   than --param=vrp-block-limit blocks (150000).  Forced here with the param.

     gcc -O2 --param=vrp-block-limit=1   does not finish in 40 s
     gcc -O2                             0.02 s

   Depth 14 / 18 / 22 measure 0.35 s / 5.4 s / >40 s: one doubling per level.
   Control: change "int r = u;" to "int r = a22;" so the queried name is the
   condition operand and gori_name_helper returns at its first test; 0.02 s.  */
extern int g1 (int);
int f (int i, int j, int k)
{
  int u = k & 3;
  int a0 = i & 255, b0 = j & 255;
  int t1 = a0 * b0; int s1 = a0 - b0;
  int a1 = t1 & 255; int b1 = s1 & 255;
  int t2 = a1 * b1; int s2 = a1 - b1;
  int a2 = t2 & 255; int b2 = s2 & 255;
  int t3 = a2 * b2; int s3 = a2 - b2;
  int a3 = t3 & 255; int b3 = s3 & 255;
  int t4 = a3 * b3; int s4 = a3 - b3;
  int a4 = t4 & 255; int b4 = s4 & 255;
  int t5 = a4 * b4; int s5 = a4 - b4;
  int a5 = t5 & 255; int b5 = s5 & 255;
  int t6 = a5 * b5; int s6 = a5 - b5;
  int a6 = t6 & 255; int b6 = s6 & 255;
  int t7 = a6 * b6; int s7 = a6 - b6;
  int a7 = t7 & 255; int b7 = s7 & 255;
  int t8 = a7 * b7; int s8 = a7 - b7;
  int a8 = t8 & 255; int b8 = s8 & 255;
  int t9 = a8 * b8; int s9 = a8 - b8;
  int a9 = t9 & 255; int b9 = s9 & 255;
  int t10 = a9 * b9; int s10 = a9 - b9;
  int a10 = t10 & 255; int b10 = s10 & 255;
  int t11 = a10 * b10; int s11 = a10 - b10;
  int a11 = t11 & 255; int b11 = s11 & 255;
  int t12 = a11 * b11; int s12 = a11 - b11;
  int a12 = t12 & 255; int b12 = s12 & 255;
  int t13 = a12 * b12; int s13 = a12 - b12;
  int a13 = t13 & 255; int b13 = s13 & 255;
  int t14 = a13 * b13; int s14 = a13 - b13;
  int a14 = t14 & 255; int b14 = s14 & 255;
  int t15 = a14 * b14; int s15 = a14 - b14;
  int a15 = t15 & 255; int b15 = s15 & 255;
  int t16 = a15 * b15; int s16 = a15 - b15;
  int a16 = t16 & 255; int b16 = s16 & 255;
  int t17 = a16 * b16; int s17 = a16 - b16;
  int a17 = t17 & 255; int b17 = s17 & 255;
  int t18 = a17 * b17; int s18 = a17 - b17;
  int a18 = t18 & 255; int b18 = s18 & 255;
  int t19 = a18 * b18; int s19 = a18 - b18;
  int a19 = t19 & 255; int b19 = s19 & 255;
  int t20 = a19 * b19; int s20 = a19 - b19;
  int a20 = t20 & 255; int b20 = s20 & 255;
  int t21 = a20 * b20; int s21 = a20 - b20;
  int a21 = t21 & 255; int b21 = s21 & 255;
  int t22 = a21 * b21; int s22 = a21 - b21;
  int a22 = t22 & 255; int b22 = s22 & 255;
  int r = u;
  if (a22 < 5) r = g1 (u);
  return r;
}
