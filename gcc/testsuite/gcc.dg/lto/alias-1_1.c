/* { dg-options "-fno-strict-aliasing" } */
extern float *ptr2;
void
typefun (float val)
{ 
  *ptr2=val;
}
