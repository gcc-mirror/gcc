/* { dg-do compile } */
/* { dg-options "-Os -ffast-math -mfpmath=387" } */

typedef __SIZE_TYPE__ size_t;
typedef struct
{
  float *ewgts;
} vtx_data;

extern void *zmalloc (size_t);
extern int whatever (vtx_data *);

float *
compute_apsp_artifical_weights_packed (vtx_data * graph, int n)
{
  float *weights;

  weights = (float *) zmalloc (n * sizeof (float));
  weights[n] =
    whatever (graph) > graph[n].ewgts[n] ?
    whatever (graph) : graph[n].ewgts[n];
}
