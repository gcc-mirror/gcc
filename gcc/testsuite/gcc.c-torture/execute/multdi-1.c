/* PR target/9348 */

#define u_l_l unsigned long long
#define l_l long long

l_l mpy_res;

u_l_l mpy (long a, long b)
{
  return (u_l_l) a * (u_l_l) b;
}
 
int main(void)
{
  mpy_res = mpy(1,-1);
  if (mpy_res != -1LL)
    abort ();
  return 0;
}

