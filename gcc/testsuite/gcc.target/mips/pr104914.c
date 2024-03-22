/* { dg-do run } */
/* { dg-options "-mabi=64" } */

extern void abort (void);
extern void exit (int);

NOMIPS16 int test (const unsigned char *buf)
{
  int val;
  ((unsigned char*)&val)[0] = *buf++;
  ((unsigned char*)&val)[1] = *buf++;
  ((unsigned char*)&val)[2] = *buf++;
  ((unsigned char*)&val)[3] = *buf++;
  if(val > 0)
    return 1;
  else
    return 0;
}

int main ()
{
  if (test("\xff\xff\xff\xff") != 0)
    abort();
  exit(0);
}
