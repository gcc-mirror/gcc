/* We were ICEing in bsi_after_labels because 
   we had a BB which did not have a lablel.
   PR middle-end/18903 */

#ifndef NO_LABEL_VALUES
void g (int s, int f)
{
  &&ff;
  s = f;
  ff:
  goto end;
  f = s;
  end:;
}
#else
int g;
#endif
