void washQtoM3(double m[9], double q[4]);
double sqrt(double);
int f(int samp)
{
      double clp[2], xyz[3], q[4], len;
      double mRF[9];
      int xi;
      for (xi=0; xi<samp; xi++)
	    {
		    q[0] = 1.0;
		    q[1] = ( ((double)(1)-(-1))*((double)((float)xi)-(-0.5)) / ((double)(samp-0.5)-(-0.5)) + (-1));
		    q[2] = ( ((double)(1)-(-1))*((double)((float)0)-(-0.5)) / ((double)(samp-0.5)-(-0.5)) + (-1));
		    q[3] = ( ((double)(1)-(-1))*((double)((float)0)-(-0.5)) / ((double)(samp-0.5)-(-0.5)) + (-1));
		    len = (sqrt((((q))[0]*((q))[0] + ((q))[1]*((q))[1] + ((q))[2]*((q))[2] + ((q))[3]*((q))[3])));
		  ((q)[0] = (q)[0]*1.0/len, (q)[1] = (q)[1]*1.0/len, (q)[2] = (q)[2]*1.0/len, (q)[3] = (q)[3]*1.0/len);
		 washQtoM3(mRF, q);
		      }
      return 0;
}
