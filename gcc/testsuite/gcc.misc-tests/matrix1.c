/*       Matrix operations      */

#define BOUND 100

int a[BOUND][BOUND],b[BOUND][BOUND],c[BOUND][BOUND];

main()
{
int i,j,k;


        
        for (i=0; i<BOUND; i++)
        {
                for (j=0; j<BOUND; j++)
                {
                        a[i][j] = 1;
                        b[i][j] = 1;
                }
        }
        for (i=0; i<BOUND; i++)
        {
                for (j=0; j<BOUND; j++)
                {
                        c[i][j] = 0;
                        for (k=0; k<BOUND; k++)
                        {
                                c[i][j] = c[i][j] + a[i][k] * b[k][j];
                        }
                }
        }
        for (i=0; i<BOUND; i++)
        {
                for (j=0; j<BOUND; j++)
                {
                        if (c[i][j] != BOUND)
                        {
                                puts("ERROR");
                                return 0;
                        }
                }
        }
	i=5;

	return 0;
}
