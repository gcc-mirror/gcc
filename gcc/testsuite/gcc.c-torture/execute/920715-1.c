double ran(int *idum);
main ()
{
  double vp = 0.0048;
  double vx;
  double vy;
  double vz;

  /* CYGNUS LOCAL -- meissner/32bit doubles */
  /* This test requires double precision, so for hosts that don't offer
     that much precision, just ignore this test.  */
  if (sizeof (double) < 8)
    exit (0);
  /* END CYGNUS LOCAL -- meissner/32bit doubles */

  maxbol(vp, &vx , &vy , &vz );
  if (vx < 0.001316505673 || vx > 0.001316505674)
    abort();
  if (vy < 0.002731492112 || vy > 0.002731492113)
    abort();
  if (vz < 0.001561454099 || vz > 0.001561454100)
    abort();
  exit(0);
}
maxbol(double vp , double *vx , double *vy , double *vz)
{
  int idum=0;
  int i;
  double temp;

  *vx=vp*ran( &idum );
  *vy=vp*ran( &idum );
  *vz=vp*ran( &idum );
}

double ran(int *idum)
{
  static long ix1,ix2,ix3;
  static double r[97];
  double temp;
  static int iff=0;
  int j;

 if(*idum<0 || iff==0){
   iff=1;
   ix1=(54773-(*idum))%259200;
   ix1=(7141*ix1+54773)%259200;
   ix2=ix1 %134456;
   ix1=(7141*ix1+54773)%259200;
   ix3=ix1 %243000;
   for(j=0; j<97; j++){
     ix1=(7141*ix1+54773)%259200;
     ix2=(8121*ix2+28411)%134456;
     r[j]=(ix1+ix2*(1.0/134456))*(1.0/259200);
   }
   *idum=1;
 }
  ix1=(7141*ix1+54773)%259200;
  ix2=(8121*ix2+28411)%134456;
  ix3=(4561*ix3+51349)%243000;
  j=((97*ix3)/243000);
  if(j >= 97 && j < 0)
    abort();
  temp=r[j];
  r[j]=(ix1+ix2*(1.0/134456))*(1.0/259200);
  return temp;
}
