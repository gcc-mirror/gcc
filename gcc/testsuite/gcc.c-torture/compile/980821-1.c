typedef int __int32_t;
int __kernel_rem_pio2(int prec)
{
	__int32_t i, jz;
	double fw, fq[20];
	switch(prec) {
	    case 2:
		fw = 0.0;
	    case 3:	 
		for (i=jz;i>0;i--) {
		    fw      = fq[i-1] +fq[i]; 
		    fq[i-1] = fw;
		}
	}
}
