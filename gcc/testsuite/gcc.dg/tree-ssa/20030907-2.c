/* PR optimization/12109

   This would ICE in tree-ssa-dce.c:process_worklist() when
   the function was expecting an SSA_NAME but found a VAR_DECL.  */

/* { dg-do compile } */
/* { dg-options "-O -ftree-dce" } */

void *do_it(void * dest, const void * src);
double *create_float(void);

void parse_rvalue(void **DataPtr)
{
  double local = 0.0;
  int terms = 1;

  *DataPtr = create_float();
  
  switch (terms)
    {
    case 1:
      *((double *)*DataPtr) = local;
      break;
      
    case 2:
      do_it(*DataPtr, &local);
      break;
    }
}

