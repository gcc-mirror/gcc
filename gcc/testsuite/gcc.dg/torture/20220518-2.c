/* { dg-do compile } */
/* { dg-additional-options "-funswitch-loops" } */

int Get_Spline_Val_sp_0, Get_Spline_Val_k;
double Get_Spline_Val_p, Get_Spline_Val_se_0_0_0;
double *Get_Spline_Val_v;
void Get_Spline_Val() {
  int i;
  for (;;)
    if (i > Get_Spline_Val_sp_0)
      Get_Spline_Val_k = Get_Spline_Val_se_0_0_0;
    else if (Get_Spline_Val_sp_0 == 1)
      Get_Spline_Val_v[Get_Spline_Val_k] = Get_Spline_Val_p;
}
