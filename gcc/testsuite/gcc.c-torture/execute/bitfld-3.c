/* Test that operations on bit-fields yield results reduced to bit-field
   type.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */

extern void exit (int);
extern void abort (void);

struct s {
  unsigned long long u33: 33;
  unsigned long long u40: 40;
  unsigned long long u41: 41;
};

struct s a = { 0x100000, 0x100000, 0x100000 };
struct s b = { 0x100000000ULL, 0x100000000ULL, 0x100000000ULL };
struct s c = { 0x1FFFFFFFFULL, 0, 0 };

int 
main (void)
{
  if (a.u33 * a.u33 != 0 || a.u33 * a.u40 != 0 || a.u40 * a.u33 != 0
      || a.u40 * a.u40 != 0)
    abort ();
  if (a.u33 * a.u41 != 0x10000000000ULL
      || a.u40 * a.u41 != 0x10000000000ULL
      || a.u41 * a.u33 != 0x10000000000ULL
      || a.u41 * a.u40 != 0x10000000000ULL
      || a.u41 * a.u41 != 0x10000000000ULL)
    abort ();
  if (b.u33 + b.u33 != 0)
    abort ();
  if (b.u33 + b.u40 != 0x200000000ULL
      || b.u33 + b.u41 != 0x200000000ULL
      || b.u40 + b.u33 != 0x200000000ULL
      || b.u40 + b.u40 != 0x200000000ULL
      || b.u40 + b.u41 != 0x200000000ULL
      || b.u41 + b.u33 != 0x200000000ULL
      || b.u41 + b.u40 != 0x200000000ULL
      || b.u41 + b.u41 != 0x200000000ULL)
    abort ();
  if (a.u33 - b.u33 != 0x100100000ULL
      || a.u33 - b.u40 != 0xFF00100000ULL
      || a.u33 - b.u41 != 0x1FF00100000ULL
      || a.u40 - b.u33 != 0xFF00100000ULL
      || a.u40 - b.u40 != 0xFF00100000ULL
      || a.u40 - b.u41 != 0x1FF00100000ULL
      || a.u41 - b.u33 != 0x1FF00100000ULL
      || a.u41 - b.u40 != 0x1FF00100000ULL
      || a.u41 - b.u41 != 0x1FF00100000ULL)
    abort ();
  if (++c.u33 != 0 || --c.u40 != 0xFFFFFFFFFFULL || c.u41-- != 0)
    abort ();
  exit (0);
}
