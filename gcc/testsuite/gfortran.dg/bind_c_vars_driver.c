double fabs (double);

/* defined in fortran module bind_c_vars */
void changeF90Globals(void);

extern void abort(void);

/* module level scope in bind_c_vars */
extern int myf90int;         /* myf90int in bind_c_vars */
float myF90Real;             /* f90_real in bind_c_vars */
int myF90Array3D[10][5][18]; /* A in bind_c_vars */
int myF90Array2D[2][3];      /* B in bind_c_vars */
int myVariable;              /* c2 in bind_c_vars */
int c3;                      /* c3 in bind_c_vars */
int c4;                      /* c4 in bind_c_vars */

int main(int argc, char **argv)
{
   myf90int = 1;
   myF90Real = 1.0;
   myVariable = 2;
   c3 = 3;
   c4 = 4;
   myF90Array3D[2][3][4] = 1;
   myF90Array2D[1][2] = 2;

   /* will change the global vars initialized above */
   changeF90Globals();

   if(myf90int != 2)
      abort();
   if(fabs(myF90Real-3.0) > 0.00000000)
      abort();
   if(myVariable != 4)
      abort();
   if(c3 != 6)
      abort();
   if(c4 != 2)
      abort();
   if(myF90Array3D[2][3][4] != 2)
      abort();
   if(myF90Array2D[1][2] != 3)
      abort();
   
   return 0;
}/* end main() */
