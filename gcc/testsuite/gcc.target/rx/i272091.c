/* { dg-do compile } */
/* { dg-options "-msmall-data-limit=100" } */

double a=6.76,b=7.34,c=0.54;
double x_1= 45.46;
static double SD_1;
static double SD_init = 45.54;
double DD_1;
double DD_init=769.0;


int main()
{
   volatile double x,y,z;
   
   x = 56.76;
   y = 4.5645;

   z = x + y;
   z = x - 4.65;
   z = 4.566 - x;
   z = x * y;
   b = 8;
   c = 34;
   return 0;
}

