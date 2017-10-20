/* The modulo constraints we generate for the niter expression
     (unsinged long)ubound - (unsigned long)lbound
   end up with a modulo that we cannot represent in the expression
   type we are using (int64_t), so we run into the codegen error
   where ISL generates a modulo/divide by sth that doesn't fit the
   type we code-generate with.  Verify we properly elide those.  */

void foo (double *a, long int lbound0, long int ubound0,
	  long int lbound1, long int ubound1, long int stride1)
{
  if (lbound0 < ubound0)
    for (long int i = lbound0; i <= ubound0; ++i)
      if (lbound1 < ubound1)
	for (long int j = lbound1; j <= ubound1; ++j)
	  a[i*stride1 + j] = 0.;
}
