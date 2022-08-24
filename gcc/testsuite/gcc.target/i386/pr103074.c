/* { dg-do compile } */
/* { dg-options "-march=bonnell -Os -fPIC -fschedule-insns -w" } */

void
serialize_collection (char *ptr, int a, int need_owner)
{
  if (need_owner)
    __builtin_sprintf(ptr, "%d:%d", 0, a);
  else
    {
      static char buff[32];

      __builtin_sprintf(buff, "%d:%d", a >> 32, a);
      __builtin_sprintf(ptr, "%d:%d:\"%s\"", 0, 0, buff);
    }
}
