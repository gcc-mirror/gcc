/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);

/* This series of tests looks for the optimization:
   x = (a >= b) + c + d
   =>
   cmp a, b
   adc x, c, d
 */

unsigned long 
ltu_add (unsigned long a, unsigned long b, unsigned long c, unsigned long d)
{
  return (a < b) + c + d;
}

unsigned long 
gtu_add (unsigned long a, unsigned long b, unsigned long c, unsigned long d)
{
  return (a > b) + c + d;
}

unsigned long 
leu_add (unsigned long a, unsigned long b, unsigned long c, unsigned long d)
{
  return (a <= b) + c + d;
}

unsigned long 
geu_add (unsigned long a, unsigned long b, unsigned long c, unsigned long d)
{
  return (a >= b) + c + d;
}

unsigned long 
equ_add (unsigned long a, unsigned long b, unsigned long c, unsigned long d)
{
  return (a == b) + c + d;
}

unsigned long 
neu_add (unsigned long a, unsigned long b, unsigned long c, unsigned long d)
{
  return (a != b) + c + d;
}

long 
lt_add ( long a,  long b,  long c,  long d)
{
  return (a < b) + c + d;
}

long 
gt_add ( long a,  long b,  long c,  long d)
{
  return (a > b) + c + d;
}

long 
le_add ( long a,  long b,  long c,  long d)
{
  return (a <= b) + c + d;
}

long 
ge_add ( long a,  long b,  long c,  long d)
{
  return (a >= b) + c + d;
}

long 
eq_add ( long a,  long b,  long c,  long d)
{
  return (a == b) + c + d;
}

long 
ne_add ( long a,  long b,  long c,  long d)
{
  return (a != b) + c + d;
}


int
main ()
{
  if (ltu_add(1,2,3,4) != 8)
    {
      abort();
    }

  if (ltu_add(2,2,3,4) != 7)
    {
      abort();
    }

  if (ltu_add(3,2,3,4) != 7)
    {
      abort();
    }

  if (gtu_add(2,1,3,4) != 8)
    {
      abort();
    }

  if (gtu_add(2,2,3,4) != 7)
    {
      abort();
    }

  if (gtu_add(1,2,3,4) != 7)
    {
      abort();
    }

  if (leu_add(1,2,3,4) != 8)
    {
      abort();
    }

  if (leu_add(2,2,3,4) != 8)
    {
      abort();
    }

  if (leu_add(3,2,3,4) != 7)
    {
      abort();
    }

  if (leu_add(2,1,3,4) != 7)
    {
      abort();
    }

  if (geu_add(2,1,3,4) != 8)
    {
      abort();
    }
  if (geu_add(2,2,3,4) != 8)
    {
      abort();
    }

  if (geu_add(1,2,3,4) != 7)
    {
      abort();
    }

  if (equ_add(1,2,3,4) != 7)
    {
      abort();
    }

  if (equ_add(2,2,3,4) != 8)
    {
      abort();
    }

  if (equ_add(3,2,3,4) != 7)
    {
      abort();
    }

  if (neu_add(1,2,3,4) != 8)
    {
      abort();
    }

  if (neu_add(2,2,3,4) != 7)
    {
      abort();
    }

  if (neu_add(3,2,3,4) != 8)
    {
      abort();
    }

  if (lt_add(1,2,3,4) != 8)
    {
      abort();
    }

  if (lt_add(2,2,3,4) != 7)
    {
      abort();
    }

  if (lt_add(3,2,3,4) != 7)
    {
      abort();
    }

  if (gt_add(2,1,3,4) != 8)
    {
      abort();
    }

  if (gt_add(2,2,3,4) != 7)
    {
      abort();
    }

  if (gt_add(1,2,3,4) != 7)
    {
      abort();
    }

  if (le_add(1,2,3,4) != 8)
    {
      abort();
    }

  if (le_add(2,2,3,4) != 8)
    {
      abort();
    }

  if (le_add(3,2,3,4) != 7)
    {
      abort();
    }

  if (le_add(2,1,3,4) != 7)
    {
      abort();
    }

  if (ge_add(2,1,3,4) != 8)
    {
      abort();
    }
  if (ge_add(2,2,3,4) != 8)
    {
      abort();
    }

  if (ge_add(1,2,3,4) != 7)
    {
      abort();
    }

  if (eq_add(1,2,3,4) != 7)
    {
      abort();
    }

  if (eq_add(2,2,3,4) != 8)
    {
      abort();
    }

  if (eq_add(3,2,3,4) != 7)
    {
      abort();
    }

  if (ne_add(1,2,3,4) != 8)
    {
      abort();
    }

  if (ne_add(2,2,3,4) != 7)
    {
      abort();
    }

  if (ne_add(3,2,3,4) != 8)
    {
      abort();
    }
  return 0;
}
