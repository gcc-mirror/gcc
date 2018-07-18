/* { dg-do compile } */
/* { dg-options "-fgimple" } */

short int __GIMPLE ()
foo (short int s)
{
  short int D_1803;

bb_2:
  D_1803 = s;

L0:
  return D_1803;

}

int __GIMPLE ()
main (int argc, char * * argv)
{
  short int s;
  int D_1805;
  int _1;
  short _2;

bb_2:
  s = (short int) argc;
  _1 = (int) s;
  _2 = foo (_1);
  D_1805 = (int) _2;

L0:
  return D_1805;
}
