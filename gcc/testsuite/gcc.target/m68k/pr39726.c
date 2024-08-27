/* { dg-do assemble } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-final { object-size text <= 228 } } */

unsigned char v;

int a0bs (unsigned char u, unsigned char w)
{
  if ((u - w) & 0x80)
    v = 1;
}

int a1bs (unsigned char u, unsigned char w)
{
  if ((u + w) & 0x80)
    v = 1;
}

int a0b (unsigned char u, unsigned char w)
{
  if ((u - w) & 0x22)
    v = 1;
}

int a1b (unsigned char u, unsigned char w)
{
  if ((u + w) & 0x22)
    v = 1;
}

int a0ws (unsigned short u, unsigned short w)
{
  if ((u - w) & 0x8000)
    v = 1;
}

int a1ws (unsigned short u, unsigned short w)
{
  if ((u + w) & 0x8000)
    v = 1;
}

int a0wbs (unsigned short u, unsigned short w)
{
  if ((u - w) & 0x80)
    v = 1;
}

int a1wbs (unsigned short u, unsigned short w)
{
  if ((u + w) & 0x80)
    v = 1;
}

int a0w (unsigned short u, unsigned short w)
{
  if ((u - w) & 0x8421)
    v = 1;
}

int a1w (unsigned short u, unsigned short w)
{
  if ((u + w) & 0x8421)
    v = 1;
}
