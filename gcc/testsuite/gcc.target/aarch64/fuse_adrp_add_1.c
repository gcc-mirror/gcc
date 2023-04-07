/* { dg-do compile } */
/* { dg-require-effective-target aarch64_small } */
/* { dg-options "-O3 -mcpu=cortex-a57 -fno-pie" } */

enum reg_class { NO_REGS, AP_REG, XRF_REGS, GENERAL_REGS, AGRF_REGS,
                 XGRF_REGS, ALL_REGS, LIM_REG_CLASSES };

enum rtx_code { REG,  LAST_AND_UNUSED_RTX_CODE };

typedef union rtunion_def
{
  int rtint;
} rtunion;

typedef struct rtx_def
{
  unsigned int volatil : 1;
  rtunion fld[1];
} *rtx;

extern char fixed_regs[64];
extern char global_regs[64];

int
rtx_cost (rtx x, int outer_code)
{
  register enum rtx_code code;
  switch (code)
    {
      case REG:
        return ! ((((x)->volatil) && ((x)->fld[0].rtint) < 64)
                 || ((((x)->fld[0].rtint)) == 30 || (((x)->fld[0].rtint)) == 30
                 || (((x)->fld[0].rtint)) == 31 || (((x)->fld[0].rtint)) == 0
                 || ((((x)->fld[0].rtint)) >= (64)
                     && (((x)->fld[0].rtint)) <= (((64)) + 3))
                 || ((((x)->fld[0].rtint)) < 64 && ((((x)->fld[0].rtint)) == 30
                 || (((x)->fld[0].rtint)) == 30 || fixed_regs[((x)->fld[0].rtint)]
                 || global_regs[((x)->fld[0].rtint)])
                    && ((((x)->fld[0].rtint))
                          ? ((((x)->fld[0].rtint) < 32)
                             ? GENERAL_REGS : XRF_REGS)
                          : AP_REG) != NO_REGS)));
    }
}

/* { dg-final { scan-assembler "adrp\tx.*, fixed_regs\n\tadd\tx.*, x.*fixed_regs" } } */
