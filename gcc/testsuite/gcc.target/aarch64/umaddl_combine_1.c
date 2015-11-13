/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=cortex-a53" } */

enum reg_class
{
  NO_REGS,
  AD_REGS,
  ALL_REGS, LIM_REG_CLASSES
};

extern enum reg_class
  reg_class_subclasses[((int) LIM_REG_CLASSES)][((int) LIM_REG_CLASSES)];

void
init_reg_sets_1 (unsigned int i)
{
  unsigned int j;
  {
    for (j = i + 1; j < ((int) LIM_REG_CLASSES); j++)
      {
	enum reg_class *p;
	p = &reg_class_subclasses[j][0];
	while (*p != LIM_REG_CLASSES)
	  p++;
      }
  }
}

/* { dg-final { scan-assembler-not "umull\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+" } } */
