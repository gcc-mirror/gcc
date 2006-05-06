/* { dg-do compile } */

/* We were creating a float vector for the vis_type == 1
   test, which we ICEd on.  Now we simply punt here.  */

float vs_data[75];
void vis_clear_data ()
{
  int vis_type, i;
  for (i = 0; i < 75; i++)
    {
      vs_data[i] = (vis_type == 1);
    }
}
