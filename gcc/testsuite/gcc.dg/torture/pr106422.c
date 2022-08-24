/* { dg-do compile } */

void vfork() __attribute__((__leaf__));
void semanage_reload_policy(char *arg, void cb(void))
{
  if (!arg)
    {
      cb();
      return;
    }
  vfork();
  if (arg)
    __builtin_free(arg);
}
