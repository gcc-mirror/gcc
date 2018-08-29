/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

short var_9 = 19581;
unsigned char var_33 = 21;
long long int var_55 = 286697804684061197LL;
long long int var_59 = -1962393262513510540LL;
long long int var_71 = 4731868609112929952LL;
long long int var_773 = -4784633456247777769LL;
short var_776 = 5894;
long long int var_1321 = 7573221950916697355LL;
unsigned char uc = 217;

void foo()
{
  if (var_55)
    var_71 = 0;
  if (var_9 != ~(0 < uc))
    var_773 = 0;
  else
    var_776 = 1 / ~var_9 * -1;
  if (var_33)
    var_59 = ~var_9 & 10393;
  var_1321 = ~var_9;
}
int main()
{
  foo();
  if (var_59 != 8320)
    __builtin_abort ();
  return 0;
}
