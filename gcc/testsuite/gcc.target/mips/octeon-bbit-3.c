/* { dg-do compile } */

/* Force big-endian because for little-endian, combine generates this:

 (if_then_else (ne (zero_extract:DI (subreg:DI (truncate:SI (reg:DI 196)) 0) 
                 (const_int 1) 
                 (const_int 0)) 
             (const_int 0)) 
         (label_ref 20) 
         (pc))) 

  which does not get recognized as a valid bbit pattern.  The
  middle-end should be able to simplify this further.  */
/* { dg-options "-O2 -march=octeon -meb" } */

/* { dg-final { scan-assembler-times "\tbbit\[01\]\t|\tbgez\t" 2 } } */
/* { dg-final { scan-assembler-not "ext\t" } } */

void abort (void);
void exit (int);

typedef unsigned long long ulong64;

typedef struct bitfield_s {
  ulong64 a:1;
  ulong64 b:29;
  ulong64 c:1;
  ulong64 d:15;
  ulong64 f:18;
} bitfield_t;

bitfield_t bar;

NOMIPS16 void
f ()
{
  foo(&bar);
  if (bar.a != 0x1)
    abort ();
  else if (!bar.c)
    abort ();
  else
    exit (0);
}
