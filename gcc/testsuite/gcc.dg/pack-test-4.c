/* PR c/11885
   Bug: flag4 was allocated into the same byte as the other flags.
   { dg-options "" }
   { dg-do run } */

extern void abort (void);

typedef unsigned char uint8_t;

typedef struct {
    uint8_t flag1:2;
    uint8_t flag2:1;
    uint8_t flag3:1;
   
    uint8_t flag4;

} __attribute__ ((packed)) MyType;

int main (void)
{
  MyType a;
  MyType *b = &a;

  b->flag1 = 0;
  b->flag2 = 0;
  b->flag3 = 0;

  b->flag4 = 0;

  b->flag4++;
    
  if (b->flag1 != 0)
    abort ();

  return 0;
}
