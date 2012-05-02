/* { dg-do compile } */
/* { dg-options "-O2" } */

extern void bar (void);

/* Case 181 is not in the range for 'char'.  */
void
foo1 (char *buf)
{
  int x = *buf;
  switch (x)
    {
    case -76:
    case 65:
    case 181:
      bar();
    }
}

/* All cases are below the range of char.  */
void
foo2 (char *buf)
{
  int x = *buf;
  switch (x)
    {
    case -150:
    case -140:
    case -130:
      bar();
    }
}

/* All cases are above the range of char.  */
void
foo3 (char *buf)
{
  int x = *buf;
  switch (x)
    {
    case 130:
    case 140:
    case 150: /* This case is not in the range for 'char'.  */
      bar();
    }
}

/* The bounding cases are partially out of range for char.  */
void
foo4 (char *buf)
{
  int x = *buf;
  switch (x)
    {
    case -130 ... -120:
    case 100:
    case 120 ... 130:
      bar();
    }
}

