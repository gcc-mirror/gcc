/* Test for ICE arising from VSX code generation.  */
/* { dg-do compile } */
/* { dg-options "-O3 -mcpu=power7 -funroll-loops" } */
/* { dg-require-effective-target powerpc_vsx_ok } */

int sourcenode;
int ARCHelems;
int *source_elms;
void
foo (int argc, char **argv)
{
  int i, j;
  int cor[4];
  double Ke[12][12], Me[12], Ce[12], Mexv[12], Cexv[12], v[12];
  for (i = 0; i < ARCHelems; i++)
    {
      for (j = 0; j < 12; j++)
	Me[j] = 0.0;
      if (cor[j] == sourcenode)
	vv12x12 (Me, v, Mexv);
      vv12x12 (Ce, v, Cexv);
      if (source_elms[i] == 3)
	for (j = 0; j < 12; j++)
	  {
	    v[j] = -v[j];
	    Mexv[j] = -Mexv[j];
	    Cexv[j] = -Cexv[j];
	  }
    }
}
