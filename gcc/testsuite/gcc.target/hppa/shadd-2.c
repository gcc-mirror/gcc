/* { dg-do compile }  */
/* { dg-options "-O2" }  */
/* { dg-final { scan-assembler-times "sh.add" 2 } }  */

typedef struct rtx_def *rtx;
typedef const struct rtx_def *const_rtx;
enum machine_mode
{
  VOIDmode, BLKmode, CCmode, CCGCmode, CCGOCmode, CCNOmode, CCAmode, CCCmode,
    CCOmode, CCSmode, CCZmode, CCFPmode, CCFPUmode, BImode, QImode, HImode,
    SImode, DImode, TImode, OImode, QQmode, HQmode, SQmode, DQmode, TQmode,
    UQQmode, UHQmode, USQmode, UDQmode, UTQmode, HAmode, SAmode, DAmode,
    TAmode, UHAmode, USAmode, UDAmode, UTAmode, SFmode, DFmode, XFmode,
    TFmode, SDmode, DDmode, TDmode, CQImode, CHImode, CSImode, CDImode,
    CTImode, COImode, SCmode, DCmode, XCmode, TCmode, V2QImode, V4QImode,
    V2HImode, V1SImode, V8QImode, V4HImode, V2SImode, V1DImode, V16QImode,
    V8HImode, V4SImode, V2DImode, V1TImode, V32QImode, V16HImode, V8SImode,
    V4DImode, V2TImode, V64QImode, V32HImode, V16SImode, V8DImode, V4TImode,
    V2SFmode, V4SFmode, V2DFmode, V8SFmode, V4DFmode, V2TFmode, V16SFmode,
    V8DFmode, V4TFmode, MAX_MACHINE_MODE, NUM_MACHINE_MODES = MAX_MACHINE_MODE
};
struct rtx_def
{
  __extension__ enum machine_mode mode:8;
};
struct target_regs
{
  unsigned char x_hard_regno_nregs[53][MAX_MACHINE_MODE];
};
extern void oof (void);
extern int rhs_regno (rtx);

extern struct target_regs default_target_regs;
__inline__ unsigned int
end_hard_regno (enum machine_mode mode, unsigned int regno)
{
  return regno +
    ((&default_target_regs)->x_hard_regno_nregs)[regno][(int) mode];
}

void
note_btr_set (rtx dest, const_rtx set
	      __attribute__ ((__unused__)), void *data)
{
  int regno, end_regno;
  end_regno = end_hard_regno (((dest)->mode), (rhs_regno (dest)));
  for (; regno < end_regno; regno++)
    oof ();
}
