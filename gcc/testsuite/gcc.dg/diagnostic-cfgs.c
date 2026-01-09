/* Verify that the "cfgs=yes" option to diagnostic sinks works.  */

/* { dg-do compile } */
/* { dg-additional-options "-fdiagnostics-add-output=sarif:cfgs=yes" } */
/* { dg-additional-options "-fdiagnostics-add-output=experimental-html:javascript=no,cfgs=yes" } */

int
test (int i, int j, int k)
{
  if (i)
    return j + k;
  else
    return j - k;
}

/* { dg-final { verify-sarif-file } }
   { dg-final { run-sarif-pytest diagnostic-cfgs.c "diagnostic-cfgs-sarif.py" } }
   { dg-final { run-html-pytest diagnostic-cfgs.c "diagnostic-cfgs-html.py" } } */
