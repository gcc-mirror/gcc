/* PR c/102989 */
/* { dg-do compile { target { bitint && { float32 && int32 } } } } */
/* { dg-options "-std=c23 -Wconversion -Wfloat-conversion" } */
/* { dg-add-options float32 } */

void
foo (_Float32 x)
{
  _BitInt(57) a = 1.5F32;				/* { dg-warning "conversion from '_Float32' to '_BitInt\\\(57\\\)' changes value from '1.5e\\\+0f32' to '1'" } */
  _BitInt(27) b = 76117358uwb;				/* { dg-warning "signed conversion from 'unsigned _BitInt\\\(27\\\)' to '_BitInt\\\(27\\\)' changes value from '76117358' to '-58100370'" } */
  unsigned _BitInt(27) c = -15wb;			/* { dg-warning "unsigned conversion from '_BitInt\\\(5\\\)' to 'unsigned _BitInt\\\(27\\\)' changes value from '-15' to '134217713'" } */
  _BitInt(27) d = -390288573wb;				/* { dg-warning "overflow in conversion from '_BitInt\\\(30\\\)' to '_BitInt\\\(27\\\)' changes value from '-390288573' to '12364611'" } */
  unsigned _BitInt(27) e = 309641337uwb;		/* { dg-warning "conversion from 'unsigned _BitInt\\\(29\\\)' to 'unsigned _BitInt\\\(27\\\)' changes value from '309641337' to '41205881'" } */
  _BitInt(27) f = 76117358U;				/* { dg-warning "signed conversion from 'unsigned int' to '_BitInt\\\(27\\\)' changes value from '76117358' to '-58100370'" } */
  unsigned _BitInt(27) g = -15;				/* { dg-warning "unsigned conversion from 'int' to 'unsigned _BitInt\\\(27\\\)' changes value from '-15' to '134217713'" } */
  _BitInt(27) h = -390288573;				/* { dg-warning "overflow in conversion from 'int' to '_BitInt\\\(27\\\)' changes value from '-390288573' to '12364611'" } */
  unsigned _BitInt(27) i = 309641337U;			/* { dg-warning "conversion from 'unsigned int' to 'unsigned _BitInt\\\(27\\\)' changes value from '309641337' to '41205881'" } */
  int j = 2936216298uwb;				/* { dg-warning "signed conversion from 'unsigned _BitInt\\\(32\\\)' to 'int' changes value from '2936216298' to '-1358750998'" } */
  unsigned int k = -15wb;				/* { dg-warning "unsigned conversion from '_BitInt\\\(5\\\)' to 'unsigned int' changes value from '-15' to '4294967281'" } */
  int l = -8087431137529383656wb;			/* { dg-warning "overflow in conversion from '_BitInt\\\(64\\\)' to 'int' changes value from '-8087431137529383656' to '-1105152744'" } */
  unsigned int m = 1664073919553255778uwb;		/* { dg-warning "conversion from 'unsigned _BitInt\\\(61\\\)' to 'unsigned int' changes value from '1664073919553255778' to '3338058082'" } */
#if __BITINT_MAXWIDTH__ >= 575
  _Float32 n = 51441631083309184313435496923626431699697406185384986811300218556561965470218425783308778801748592322915101142266821623326688106425864884688172114173397118407357447763009120wb;	/* { dg-warning "conversion from '_BitInt\\\(575\\\)' to '_Float32' changes value from '0x353eab28b46b03ea99b84f9736cd8dbe5e986915a0383c3cb381c0da41e31b3621c75fd53262bfcb1b0e6251dbf00f3988784e29b08b65640c263e4d0959832a20e2ff5245be1e60' to '\\\+Inff32'" "" { target bitint575 } } */
#endif
  _BitInt(57) o = x;					/* { dg-warning "conversion from '_Float32' to '_BitInt\\\(57\\\)' may change value" } */
  unsigned _BitInt(15) p = 32767uwb;
  unsigned _BitInt(15) q = (_BitInt(42)) p;
  _BitInt(17) r = 0;
  _BitInt(17) s = ((_BitInt(42)) r) & 32767wb;
#if __BITINT_MAXWIDTH__ >= 575
  _BitInt(575) t = 0;
  _Float32 u = t;					/* { dg-warning "conversion from '_BitInt\\\(575\\\)' to '_Float32' may change value" "" { target bitint575 } } */
#endif
  _BitInt(42) v = 0;
  unsigned _BitInt(17) w = v;				/* { dg-warning "conversion from '_BitInt\\\(42\\\)' to 'unsigned _BitInt\\\(17\\\)' may change value" } */
  _BitInt(17) y = v;					/* { dg-warning "conversion from '_BitInt\\\(42\\\)' to '_BitInt\\\(17\\\)' may change value" } */
}
