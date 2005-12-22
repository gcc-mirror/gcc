/* PR objc/25328 */

int
main ()
{
  int status = 0;
  char msg[100] = "";
  if (__builtin_strcmp (msg, ""))
    status = 200;
  return status;
}
