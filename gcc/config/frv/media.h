/*      VENUS Family C Library V40L00                                   */
/*      COPYRIGHT(C) FUJITSU LIMITED 1993-1999                          */

#ifndef __MEDIA_H__
#define __MEDIA_H__

#ifdef __STDC__
#define __MEDIA_PASTE__(A,B) __MEDIA_XPASTE__(A,B)
#define __MEDIA_XPASTE__(A,B) A ## B
#else
#define __MEDIA_PASTE__(A,B) A/**/B
#endif

/* Floating Point Condition Code Field Type */
typedef enum
{
  FCC0 = 0,
  FCC1,
  FCC2, 
  FCC3
} FCC_T;

/* Accumulator Type */
#define ACC0	0
#define ACC1	1
#define ACC2	2
#define ACC3	3
#define ACC4	4
#define ACC5	5
#define ACC6	6
#define ACC7	7

typedef unsigned char      __mubyte;
typedef unsigned short	   __muhalf;
typedef unsigned long      __muword1;
typedef unsigned long long __muword2;

typedef signed short	   __mshalf;
typedef signed long        __msword1;
typedef signed long long   __msword2;

typedef int ACC_T;

register __muword1 __acc0 __asm__("acc0");
register __muword1 __acc1 __asm__("acc1");
register __muword1 __acc2 __asm__("acc2");
register __muword1 __acc3 __asm__("acc3");
register __muword1 __acc4 __asm__("acc4");
register __muword1 __acc5 __asm__("acc5");
register __muword1 __acc6 __asm__("acc6");
register __muword1 __acc7 __asm__("acc7");

#define __ACC(N) __MEDIA_PASTE__(__acc,N)

/* Accumulator Guard Type */
#define ACCG0	0
#define ACCG1	1
#define ACCG2	2
#define ACCG3	3
#define ACCG4	4
#define ACCG5	5
#define ACCG6	6
#define ACCG7	7

typedef int ACCG_T;

register __mubyte __accg0 __asm__("accg0");
register __mubyte __accg1 __asm__("accg1");
register __mubyte __accg2 __asm__("accg2");
register __mubyte __accg3 __asm__("accg3");
register __mubyte __accg4 __asm__("accg4");
register __mubyte __accg5 __asm__("accg5");
register __mubyte __accg6 __asm__("accg6");
register __mubyte __accg7 __asm__("accg7");

#define __ACCG(N) __MEDIA_PASTE__(__accg,N)

/* 12-bit Immediate Type */
typedef int IMM12;

/* 6-bit Immediate Type */
typedef int IMM6;

/* 5-bit Immediate Type */
typedef int IMM5;

/* 5-bit Unsigned Immediate Type */
typedef int UIMM5;

/* 4-bit Unsigned Immediate Type */
typedef int UIMM4;

/* 1-bit Unsigned Immediate Type */
typedef int UIMM1;

/* Media Logical (Word) */
extern __muword1 __MAND(__muword1, __muword1);
extern __muword1 __MOR(__muword1, __muword1);
extern __muword1 __MXOR(__muword1, __muword1);
extern __muword1 __MNOT(__muword1);

/* Media Rotate (Word) */
extern __muword1 __MROTLI(__muword1, UIMM5);
extern __muword1 __MROTRI(__muword1, UIMM5);

/* Media Word Cut */
extern __muword1 __MWCUT(__muword2, __muword1);

/* Media Average (Halfword Dual) */
extern __muword1 __MAVEH(__muword1, __muword1);

/* Media Shift (Halfword Dual) */
extern __muword1 __MSLLHI(__muword1, UIMM4);
extern __muword1 __MSRLHI(__muword1, UIMM4);
extern __msword1 __MSRAHI(__msword1, UIMM4);

/* Media Saturation (Halfword Dual) */
extern __msword1 __MSATHS(__msword1, __msword1);
extern __muword1 __MSATHU(__muword1, __muword1);

#if 0 /* These are not supported. */
/* Media Dual Compare (Halfword Dual) */
extern void __MCMPSH(FCC_T, __msword1, __msword1);
extern void __MCMPUH(FCC_T, __muword1, __muword1);
#endif

/* Media Dual Saturation Add/Sub (Halfword Dual) */
extern __msword1 __MADDHSS(__msword1, __msword1);
extern __muword1 __MADDHUS(__muword1, __muword1);
extern __msword1 __MSUBHSS(__msword1, __msword1);
extern __muword1 __MSUBHUS(__muword1, __muword1);

/* Media Dual Mult (Halfword Dual) */
extern void __MMULHS(ACC_T, __msword1, __msword1);
extern void __MMULHU(ACC_T, __muword1, __muword1);

/* Media Dual Cross Mult (Halfword Dual) */
extern void __MMULXHS(ACC_T, __msword1, __msword1);
extern void __MMULXHU(ACC_T, __muword1, __muword1);

/* Media Dual Mult & Add (Halfword Dual) */
extern void __MMACHS(ACC_T, __msword1, __msword1);
extern void __MMACHU(ACC_T, __muword1, __muword1);

/* Media Dual Mult & Sub (Halfword Dual) */
extern void __MMRDHS(ACC_T, __msword1, __msword1);
extern void __MMRDHU(ACC_T, __muword1, __muword1);

/* Media Quad Saturation Add/Sub (Halfword Quad) */
extern __msword2 __MQADDHSS(__msword2, __msword2);
extern __muword2 __MQADDHUS(__muword2, __muword2);
extern __msword2 __MQSUBHSS(__msword2, __msword2);
extern __muword2 __MQSUBHUS(__muword2, __muword2);

/* Media Quad Mult (Halfword Quad) */
extern void __MQMULHS(ACC_T, __msword2, __msword2);
extern void __MQMULHU(ACC_T, __muword2, __muword2);

/* Media Quad Cross Mult (Halfword Quad) */
extern void __MQMULXHS(ACC_T, __msword2, __msword2);
extern void __MQMULXHU(ACC_T, __muword2, __muword2);

/* Media Quad Mult & Add (Halfword Quad) */
extern void __MQMACHS(ACC_T, __msword2, __msword2);
extern void __MQMACHU(ACC_T, __muword2, __muword2);

/* Media Dual Mult & Add for Complex (Halfword Dual) */
extern void __MCPXRS(ACC_T, __msword1, __msword1);
extern void __MCPXRU(ACC_T, __muword1, __muword1);
extern void __MCPXIS(ACC_T, __msword1, __msword1);
extern void __MCPXIU(ACC_T, __muword1, __muword1);

/* Media Quad Mult & Add for Complex (Halfword Quad) */
extern void __MQCPXRS(ACC_T, __msword2, __msword2);
extern void __MQCPXRU(ACC_T, __muword2, __muword2);
extern void __MQCPXIS(ACC_T, __msword2, __msword2);
extern void __MQCPXIU(ACC_T, __muword2, __muword2);

/* Media Cut */
extern __muword1 __MCUT(ACC_T, __muword1);
extern __muword1 __MCUTSS(ACC_T, __msword1);

/* Media Halfword Expand */
extern __muword1 __MEXPDHW(__muword1, UIMM1);
extern __muword2 __MEXPDHD(__muword1, UIMM1);

/* Media Halfword Pack/Unpack */
extern __muword1 __MPACKH(__muhalf, __muhalf);
extern __muword2 __MUNPACKH(__muword1);

/* Media Halfword Pack/Unpack (Dual) */
extern __muword2 __MDPACKH(__muword2, __muword2);
extern void __MDUNPACKH(__muword1[4], __muword2);

/* Media Byte-Halfword Convert */
extern __muword2 __MBTOH(__muword1);
extern __muword1 __MHTOB(__muword2);
extern void __MBTOHE(__muword1[4], __muword1);

/* Media Accumulator Clear */
extern void __MCLRACC(ACC_T);      
extern void __MCLRACCA(void);

/* Media Accumlator Read/Write */
extern __muword1 __MRDACC(ACC_T);
extern __muword1 __MRDACCG(ACCG_T);
extern void __MWTACC(ACC_T, __muword1);
extern void __MWTACCG(ACCG_T, __muword1);

/* Media Custom */
extern __muword1 __Mcop1(__muword1, __muword1);
extern __muword1 __Mcop2(__muword1, __muword1);

/* Media Trap */
extern void __MTRAP(void);

/* The following are available on the FR400.  The compiler will report an
   error if an attempt is made to use them in FR500 code.  */

/* Media Multiply And Add (Halfword) */
extern void __MQXMACHS(ACC_T, __msword2, __msword2);
extern void __MQXMACXHS(ACC_T, __msword2, __msword2);
extern void __MQMACXHS(ACC_T, __msword2, __msword2);

/* Media Accumulator Addition/Subtraction */
extern void __MADDACCS(ACC_T, ACC_T);
extern void __MSUBACCS(ACC_T, ACC_T);
extern void __MASACCS(ACC_T, ACC_T);
extern void __MDADDACCS(ACC_T, ACC_T);
extern void __MDSUBACCS(ACC_T, ACC_T);
extern void __MDASACCS(ACC_T, ACC_T);

/* Media Dual Absolute (Halfword) */
extern __muword1 __MABSHS(__msword1);

/* Media Dual Rotate Left */
extern __muword2 __MDROTLI(__muword2, UIMM5);

/* Media Dual Coupling */
extern __muword1 __MCPLHI(__muword2, UIMM4);
extern __muword1 __MCPLI(__muword2, UIMM5);

/* Media Dual Cut And Signed Saturation */
extern __muword2 __MDCUTSSI(ACC_T, IMM6);

/* Media Quad Saturation (Halfword) */
extern __msword2 __MQSATHS(__msword2, __msword2);

/* Media SETHI/SETLO */
extern __msword1 __MHSETLOS(__msword1, IMM12);
extern __msword1 __MHSETHIS(__msword1, IMM12);
extern __msword1 __MHDSETS(IMM12);
extern __muword1 __MHSETLOH(__muword1, IMM5);
extern __muword1 __MHSETHIH(__muword1, IMM5);
extern __muword1 __MHDSETH(__muword1, IMM5);
#endif /* __MEDIA_H__ */
