/* { dg-do compile } */
/* { dg-options "-march=rv64gc_xtheadbb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Os" "-Og" "-Oz" "-flto" } } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
**li_th_srri_1:
**     li	a[0-9]+,-18
**     th.srri	a[0-9]+,a[0-9]+,21
**     ret
*/
long
li_th_srri_1 (void)
{
  return 0xffff77ffffffffffL;
}

/*
**li_th_srri_2:
**     li	a[0-9]+,-18
**     th.srri	a[0-9]+,a[0-9]+,5
**     ret
*/
long
li_th_srri_2 (void)
{
  return 0x77ffffffffffffffL;
}

/*
**li_th_srri_3:
**     li	a[0-9]+,-18
**     th.srri	a[0-9]+,a[0-9]+,36
**     ret
*/
long
li_th_srri_3 (void)
{
  return 0xfffffffeefffffffL;
}

/*
**li_th_srri_4:
**     li	a[0-9]+,-86
**     th.srri	a[0-9]+,a[0-9]+,3
**     ret
*/
long
li_th_srri_4 (void)
{
  return 0x5ffffffffffffff5L;
}

/*
**li_th_srri_5:
**     li	a[0-9]+,-86
**     th.srri	a[0-9]+,a[0-9]+,4
**     ret
*/
long
li_th_srri_5 (void)
{
  return 0xaffffffffffffffaL;
}

/*
**li_th_srri_6:
**     li	a[0-9]+,-256
**     th.srri	a[0-9]+,a[0-9]+,40
**     ret
*/
long
li_th_srri_6 (void)
{
  return 0xffffffff00ffffffL;
}

/*
**li_th_srri_7:
**     li	a[0-9]+,-2048
**     th.srri	a[0-9]+,a[0-9]+,16
**     ret
*/
long
li_th_srri_7 (void)
{
  return 0xf800ffffffffffffL;
}
