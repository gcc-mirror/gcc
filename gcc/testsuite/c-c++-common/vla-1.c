/* Test that changes to a variable are reflected in a VLA later in the
   expression.  */
/* { dg-options "" } */

#ifdef __cplusplus
extern "C"
#endif
void abort();

int i = 4;
int f()
{
  return i;
}

int main()
{
  if (i+=2, sizeof(*(int(*)[f()])0) != 6*sizeof(int))
    abort();
  return 0;
}
