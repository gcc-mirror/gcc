/* { dg-lto-do run } */
/* { dg-require-effective-target mpx } */
/* { dg-lto-options { { -O2 -flto -fcheck-pointer-bounds -mmpx -nodefaultlibs -lc } } } */

int glob = 1;

void __attribute__((constructor))
ctor1 ()
{
  glob += 1;
}


void __attribute__((constructor))
ctor2 ()
{
  glob -= 2;
}

int main (int argc, const char **argv)
{
  return glob;
}
