/* This would cause PRE load motion to generate invalid code and ICE */
void foo (char *name)
{
  if (*name)
    name ++;
  while (name[0]);
  asm ("" : "=r" (name));
}
