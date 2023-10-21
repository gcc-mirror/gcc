/* { dg-additional-options "-std=gnu89" } */

static const char default_tupleseps[] = ", \t";


fubar (tupleseps)
     const char *tupleseps;
{
  char *kp, *sp;
  const char *septmp;
  const char *tseplist;
  tseplist = (tupleseps) ? tupleseps : default_tupleseps;
  while (kp)
    {
      if (*tseplist)
        septmp = tseplist;
      bar (*septmp);
      if (*tseplist)
        if (*kp)
          ;
    }
}
