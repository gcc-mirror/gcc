/* { dg-do run } */
/* { dg-options "-O3 -funroll-loops" } */
typedef struct rowbox {
    int startx ;
    int endx ;
    int endx1 ;
    int startx2 ;
    int ypos ;
    int desiredL ;
} ROWBOX ;
ROWBOX rowArray1[3] ;
ROWBOX *rowArray = rowArray1;

int numRows = 2;

int row = 1;
int block = 0;
double ckt_size_factor ;

__attribute__ ((noinline))
int
configure2()
{
  block = 0 ;
  for( row = 1 ; row <= numRows ; row++ ) {
      block++ ;
    if( rowArray[row].endx1 > 0 ) {
      block++ ;
    }
  }
}

int
main()
{
  configure2();
  __builtin_exit (0);
}
