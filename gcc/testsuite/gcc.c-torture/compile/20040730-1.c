/*  PR tree-opt/16827
    This used to ICE in tree-ssa-loop-im.c */

extern unsigned short dev_roles[];
void super_1_sync(int *rdev2)
{
 int i;
 int max_dev = 0;

 for (i =0;i<20;i++)
  if (rdev2[i] > max_dev)
   max_dev = rdev2[i];

 for (i=0; i<max_dev;i++)
  dev_roles[max_dev] = 0xfffe;

}
