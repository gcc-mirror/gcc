#include <stdlib.h>
#include "tree-vect.h"

typedef struct {
    short I;
    int W;
    int X;
    int V;
    int U;
    int P;
    int Q;
    int R;
} __attribute__((packed)) f1_neuron;

f1_neuron *f1_layer;

int numf1s = 1000;

void __attribute__((noinline,noclone))
reset_nodes() 
{
  int i;

  for (i=0;i<numf1s;i++)
    {
      f1_layer[i].W = 0;
      f1_layer[i].X = 0;
      f1_layer[i].V = 0;
      f1_layer[i].U = 0;
      f1_layer[i].P = 0;
      f1_layer[i].Q = 0;
      f1_layer[i].R = 0;
    }
}

int main ()
{
  int i;
  check_vect ();
  f1_layer = (f1_neuron *)malloc (numf1s * sizeof (f1_neuron));
  for (i = 0; i < numf1s; i++)
    f1_layer[i].I = -1;
  reset_nodes ();
#pragma GCC novector
  for (i = 0; i < numf1s; i++)
    if (f1_layer[i].I != -1)
      abort ();
  return 0; 
}
