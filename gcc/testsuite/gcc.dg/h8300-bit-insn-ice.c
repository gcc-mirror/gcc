/* { dg-skip-if "" { "h8300*-*-*" } "*" "-msx*" }  */
/* ICE for bit instruction generation using 16-bit const */

__extension__ struct st_mstp
{
  union
  {
    unsigned short WORD;
    struct
    {
      unsigned char ACSE:1;
      unsigned char _EXDMAC:1;
      unsigned char _DMAC:1;
      unsigned char _DTC:1;
      unsigned char:2;
      unsigned char _TMR23:1;
      unsigned char _TMR01:1;
      unsigned char:2;
      unsigned char _DA:1;
      unsigned char:1;
      unsigned char _AD:1;
      unsigned char:1;
      unsigned char _TPUU:1;
      unsigned char _TPUL:1;
    } BIT;
  } CRA;
};
#define MSTP    (*(volatile struct st_mstp  *)0xFFFDC8)
#define MSTPA_EXDMA    0x4000
#define MSTPA_AND      0xFEFF

int
main ()
{
  MSTP.CRA.WORD |= MSTPA_EXDMA;
  MSTP.CRA.WORD ^= MSTPA_EXDMA;
  MSTP.CRA.WORD &= MSTPA_AND;
  return 0;
}
