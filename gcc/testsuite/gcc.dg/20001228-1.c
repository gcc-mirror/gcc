/* { dg-do compile } */
/* { dg-options "-ffast-math -O2" } */

typedef struct
{
  float vs_data[75], vs_peak[75], vs_peak_speed[75];
  int vs_refresh_delay;
  int vs_doublesize;
} Vis;
 
void vis_timeout_func(Vis * vis)
{
  if (vis->vs_peak[0] < 0.0)
    vis->vs_peak[0] = 0.0;
} 

