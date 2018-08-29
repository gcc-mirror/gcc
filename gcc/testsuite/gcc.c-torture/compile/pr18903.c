/* { dg-require-effective-target label_values } */

/* We were ICEing in bsi_after_labels because 
   we had a BB which did not have a lablel.
   PR middle-end/18903 */

void g (int s, int f)
{
  &&ff;
  s = f;
  ff:
  goto end;
  f = s;
  end:;
}
