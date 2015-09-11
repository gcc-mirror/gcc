/* DN(X) generates 2**N copies of asm instruction X.  */
#define D0(X) X
#define D1(X) X "\n\t" X
#define D2(X) D1 (D1 (X))
#define D3(X) D2 (D1 (X))
#define D4(X) D2 (D2 (X))
#define D5(X) D4 (D1 (X))
#define D6(X) D4 (D2 (X))
#define D7(X) D4 (D2 (D1 (X)))
#define D8(X) D4 (D4 (X))
#define D9(X) D8 (D1 (X))
#define D10(X) D8 (D2 (X))
#define D11(X) D8 (D2 (D1 (X)))
#define D12(X) D8 (D4 (X))
#define D13(X) D8 (D4 (D1 (X)))
#define D14(X) D8 (D4 (D2 (X)))

/* Emit something that is 0x1fff8 bytes long, which is the largest
   permissible range for non-MIPS16 forward branches.  */
#define OCCUPY_0x1fff8 \
  asm (D14 ("nop") "\n\t" \
       D13 ("nop") "\n\t" \
       D12 ("nop") "\n\t" \
       D11 ("nop") "\n\t" \
       D10 ("nop") "\n\t" \
       D9 ("nop") "\n\t" \
       D8 ("nop") "\n\t" \
       D7 ("nop") "\n\t" \
       D6 ("nop") "\n\t" \
       D5 ("nop") "\n\t" \
       D4 ("nop") "\n\t" \
       D3 ("nop") "\n\t" \
       D2 ("nop") "\n\t" \
       D1 ("nop"))

/* Emit something that is 0xfffc bytes long, which is the largest
   permissible range for microMIPS forward branches when branches
   have delay slots.  */
#define OCCUPY_0xfffc \
  asm (D13 ("nop32") "\n\t" \
       D12 ("nop32") "\n\t" \
       D11 ("nop32") "\n\t" \
       D10 ("nop32") "\n\t" \
       D9 ("nop32") "\n\t" \
       D8 ("nop32") "\n\t" \
       D7 ("nop32") "\n\t" \
       D6 ("nop32") "\n\t" \
       D5 ("nop32") "\n\t" \
       D4 ("nop32") "\n\t" \
       D3 ("nop32") "\n\t" \
       D2 ("nop32") "\n\t" \
       D1 ("nop32") "\n\t" \
       D0 ("nop32"))
/* Likewise emit something that is 0x1fffc bytes long.  */
#define OCCUPY_0x1fffc do { asm ("nop"); OCCUPY_0x1fff8; } while (0)
/* Likewise emit something that is 0x10000 bytes long.  */
#define OCCUPY_0x10000 do { asm ("nop32"); OCCUPY_0xfffc; } while (0)
