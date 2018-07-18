/* { dg-add-options stack_size } */

#if defined(STACK_SIZE) && (STACK_SIZE < 65536)
# define HISTO_SIZE 9
#else
# define HISTO_SIZE 36
#endif

extern int N;
extern int nrows;
extern int or_num_angles;

typedef struct
{
  double value;
  int count;
}Histo;

Histo add_histo[10][2][HISTO_SIZE][HISTO_SIZE];

void cmd_connection_statistics( )
{
  int i,j,k,m;

  for(i=0; i<nrows; i++){
      for(j=0; j< 2; j++)
	for(k=0; k< or_num_angles; k++)
	;
  }
}
