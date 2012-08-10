/* { dg-do run } */
/* { dg-options "-fwhole-program" } */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

void mem_init (void);
int ARCHnodes, ARCHnodes1;
int ***vel;

/* The whole matrix VEL is flattened (3 dimensions).  
   The two inner dimensions are transposed.  
                                    dim 1 -> dim 2
                                    dim 2 -> dim 1
*/
/*--------------------------------------------------------------------------*/

int
main (int argc, char **argv)
{
  int i, j, k;

  ARCHnodes = 2;
  ARCHnodes1 = 4;

/* Dynamic memory allocations and initializations */

  mem_init ();

  for (j = 0; j < 4; j++)
    {
      for (i = 0; i < 2; i++)
	{
	  for (k = 0; k < 3; k++)
	{
	    printf ("[%d][%d][%d]=%d ", i, j, k, vel[i][k][j]);
	}
	  printf ("\n");
	}
      printf ("\n");
    }
  for (i = 0; i < 2; i++)
    for (j = 0; j < 3; j++)
      free (vel[i][j]);

  for (i = 0; i < 2; i++)
    free (vel[i]);

  free (vel);
  return 0;
}

/*--------------------------------------------------------------------------*/
/* Dynamic memory allocations and initializations                           */

void
mem_init (void)
{

  signed int i, j, k,d;
 
  d = 0;
  vel = (int ***) malloc (ARCHnodes * sizeof (int **));

  for (i = 0; i < ARCHnodes; i++)
    {
      vel[i] = (int **) malloc (3 * sizeof (int *));
      if (vel[i] == (int **) NULL)
	{
	  fprintf (stderr, "malloc failed for vel[%d]\n", i);
	  fflush (stderr);
	  exit (0);
	}
    }
  for (i = 0; i < ARCHnodes; i++)
    {
      for (j = 0; j < 3; j++)
	{
	  vel[i][j] = (int *) malloc (ARCHnodes1 * sizeof (int));
	}
    }
  for (i = 0; i < ARCHnodes; i++)
    {
      for (j = 0; j < 3; j++)
	{
	  for (k = 0; k < ARCHnodes1; k++)
	    {
              printf ("acc to dim2 ");
	      vel[i][j][k] = d;
	      d++;
	    }
	}
    }
  printf ("\n");

}

/*--------------------------------------------------------------------------*/
