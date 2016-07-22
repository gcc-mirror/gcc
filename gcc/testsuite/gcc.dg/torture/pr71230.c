/* { dg-do compile } */
/* { dg-additional-options "-ffast-math" } */

void metric_carttosphere(int *cctk_lsh, double txz, double tyz, double txx,
			 double tzz, double sint, double cosp, double cost,
			 double tyy, double sinp, double txy, double *grp,
			 double *grq, double *r)
{
  int i;
  for(i=0; i<cctk_lsh[0]*cctk_lsh[1]*cctk_lsh[2]; i++)
    {
      grq[i] = (cost*tyy*((sinp)*(sinp))*sint+
		2*cosp*cost*txy*sinp*sint-
		cost*tzz*sint+ 
		((cosp)*(cosp))*cost*txx*sint+
		2*((cost)*(cost))*tyz*sinp-
		tyz*sinp+
		2*cosp*((cost)*(cost))*txz-
		cosp*txz)*r[i];
      grp[i] = ((-txy*((sinp)*(sinp))+
		 (cosp*tyy-cosp*txx)*sinp+
		 ((cosp)*(cosp))*txy)*sint-
		cost*txz*sinp+cosp*cost*tyz);
    }
}
