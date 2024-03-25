/* { dg-do compile }  */
/* { dg-options "-mh -O2 -w" }  */
/* { dg-final { scan-assembler-times ":8" 2 } }  */


struct x {
  char x;
  char y;
};

void oof (void);

struct x __attribute__ ((eightbit_data)) foo;

int bar ()
{
 if ((foo.y & 0x80) != 0)
   oof ();
}

int com ()
{
 if ((foo.x & 0x80) != 0)
   oof ();
}
