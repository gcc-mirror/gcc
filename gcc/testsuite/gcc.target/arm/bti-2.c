/* { dg-do compile } */
/* -Os to create jump table.  */
/* { dg-options "-Os" } */
/* { dg-require-effective-target arm_arch_v8_1m_main_ok } */
/* { dg-add-options arm_arch_v8_1m_main } */
/* { dg-additional-options "-mfloat-abi=softfp -mbranch-protection=bti --save-temps" } */

extern int f1 (void);
extern int f2 (void);
extern int f3 (void);
extern int f4 (void);
extern int f5 (void);
extern int f6 (void);
extern int f7 (void);
extern int f8 (void);
extern int f9 (void);
extern int f10 (void);

int (*ptr) (void);

int
f_jump_table (int y, int n)
{
  int i;
  for (i = 0; i < n ;i ++)
  {
    switch (y)
      {
      case 0 : ptr = f1; break;
      case 1 : ptr = f2; break;
      case 2 : ptr = f3; break;
      case 3 : ptr = f4; break;
      case 4 : ptr = f5; break;
      case 5 : ptr = f6; break;
      case 6 : ptr = f7; break;
      case 7 : ptr = f8; break;
      case 8 : ptr = f9; break;
      case 9 : ptr = f10; break;
      default: break;
      }
    y += ptr ();
  }
  return (y == 0)? y+1:4;
}

int
f_label_address ()
{
  static void * addr = &&lab1;
  goto *addr;
lab1:
  addr = &&lab2;
  return 1;
lab2:
  addr = &&lab1;
  return 2;
}

/* { dg-final { scan-assembler-times "\tbti" 14 } } */
