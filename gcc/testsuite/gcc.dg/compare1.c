/* Test for a bogus warning on comparison between signed and unsigned.
   This was inspired by code in gcc. */

/* { dg-do compile } */
/* { dg-options "-Wsign-compare" } */

int target_flags = 1;

enum machine_mode 
{
  VOIDmode , PQImode , QImode , PHImode , HImode ,
  PSImode , SImode , PDImode , DImode , TImode , OImode , QFmode ,
  HFmode , TQFmode , SFmode , DFmode , XFmode , TFmode , QCmode ,
  HCmode , SCmode , DCmode , XCmode , TCmode , CQImode , CHImode ,
  CSImode , CDImode , CTImode , COImode , BLKmode , CCmode , CCXmode,
  CC_NOOVmode, CCX_NOOVmode, CCFPmode, CCFPEmode , MAX_MACHINE_MODE 
};

#define Pmode ( target_flags ? DImode : SImode )

int main()
{
  enum machine_mode mode = DImode;

  return (mode == Pmode); /* dg-bogus "warning:" "comparison between signed and unsigned" } */
}
