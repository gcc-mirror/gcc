extern int N;
extern int nrows;
extern int or_num_angles;

typedef struct
{
  double value;
  int count;
}Histo;

Histo add_histo[10][2][36][36];

void cmd_connection_statistics( )
{
  int i,j,k,m;

  for(i=0; i<nrows; i++){
      for(j=0; j< 2; j++)
	for(k=0; k< or_num_angles; k++)
	;
  }
}
