/* { dg-do compile } */
/* { dg-options "-O3 -Wall" } */
/* based on PR 37861 */

extern int printf (__const char *__restrict __format, ...);

static int f2(char formatstr[10][100])  
{
  int anz;
  for( anz = 0; anz < 10; ++anz ) {
    printf( "%d %s\n", anz, formatstr[anz] );
  } 
  return anz;
}


static   char formatstr[10][100];
int main( void ) 
{
  int anz;
  anz = f2(formatstr);
  printf( "   %d\n",anz);
  return 0;
}
