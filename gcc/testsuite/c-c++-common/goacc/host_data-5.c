/* { dg-do compile } */

#define N 1024

int main (int argc, char* argv[])
{
  int x[N];

#pragma acc data copyin (x[0:N])
  {
    int *xp;
#pragma acc host_data use_device (x)
    {
      /* This use of the present clause is undefined behavior for OpenACC.  */
#pragma acc parallel present (x) copyout (xp) /* { dg-error "variable 'x' declared in enclosing 'host_data' region" } */
      {
        xp = x;
      }
    }
  }

  return 0;
}
