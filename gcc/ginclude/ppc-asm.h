/* PowerPC asm definitions for GNU C.  */
/* Under winnt, 1) gas supports the following as names and 2) in particular
   defining "toc" breaks the FUNC_START macro as ".toc" becomes ".2" */

#if !defined(__WINNT__)
#define r0	0
#define sp	1
#define toc	2
#define r3	3
#define r4	4
#define r5	5
#define r6	6
#define r7	7
#define r8	8
#define r9	9
#define r10	10
#define r11	11
#define r12	12
#define r13	13
#define r14	14
#define r15	15
#define r16	16
#define r17	17
#define r18	18
#define r19     19
#define r20	20
#define r21	21
#define r22	22
#define r23	23
#define r24	24
#define r25	25
#define r26	26
#define r27	27
#define r28	28
#define r29	29
#define r30	30
#define r31	31

#define cr0	0
#define cr1	1
#define cr2	2
#define cr3	3
#define cr4	4
#define cr5	5
#define cr6	6
#define cr7	7

#define f0	0
#define f1	1
#define f2	2
#define f3	3
#define f4	4
#define f5	5
#define f6	6
#define f7	7
#define f8	8
#define f9	9
#define f10	10
#define f11	11
#define f12	12
#define f13	13
#define f14	14
#define f15	15
#define f16	16
#define f17	17
#define f18	18
#define f19     19
#define f20	20
#define f21	21
#define f22	22
#define f23	23
#define f24	24
#define f25	25
#define f26	26
#define f27	27
#define f28	28
#define f29	29
#define f30	30
#define f31	31
#endif

/*
 * Macros to glue together two tokens.
 */

#ifdef __STDC__
#define XGLUE(a,b) a##b
#else
#define XGLUE(a,b) a/**/b
#endif

#define GLUE(a,b) XGLUE(a,b)

/*
 * Macros to begin and end a function written in assembler.  If -mcall-aixdesc
 * or -mcall-nt, create a function descriptor with the given name, and create
 * the real function with one or two leading periods respectively.
 */

#ifdef _RELOCATABLE
#define DESC_SECTION ".got2"
#else
#define DESC_SECTION ".got1"
#endif

#if defined(_CALL_AIXDESC)
#define FUNC_NAME(name) GLUE(.,name)
#define FUNC_START(name) \
	.section DESC_SECTION,"aw"; \
name: \
	.long GLUE(.,name); \
	.long _GLOBAL_OFFSET_TABLE_; \
	.long 0; \
	.previous; \
	.type GLUE(.,name),@function; \
	.globl name; \
	.globl GLUE(.,name); \
GLUE(.,name):

#define FUNC_END(name) \
GLUE(.L,name): \
	.size GLUE(.,name),GLUE(.L,name)-GLUE(.,name)

#elif defined(__WINNT__)
#define FUNC_NAME(name) GLUE(..,name)
#define FUNC_START(name) \
	.pdata; \
	.align 2; \
	.ualong GLUE(..,name),GLUE(name,.e),0,0,GLUE(..,name); \
	.reldata; \
name: \
	.ualong GLUE(..,name),.toc; \
	.section .text; \
	.globl name; \
	.globl GLUE(..,name); \
GLUE(..,name):

#define FUNC_END(name) \
GLUE(name,.e): ; \
GLUE(FE_MOT_RESVD..,name):

#elif defined(_CALL_NT)
#define FUNC_NAME(name) GLUE(..,name)
#define FUNC_START(name) \
	.section DESC_SECTION,"aw"; \
name: \
	.long GLUE(..,name); \
	.long _GLOBAL_OFFSET_TABLE_; \
	.previous; \
	.type GLUE(..,name),@function; \
	.globl name; \
	.globl GLUE(..,name); \
GLUE(..,name):

#define FUNC_END(name) \
GLUE(.L,name): \
	.size GLUE(..,name),GLUE(.L,name)-GLUE(..,name)

#else
#define FUNC_NAME(name) name
#define FUNC_START(name) \
	.type name,@function; \
	.globl name; \
name:

#define FUNC_END(name) \
GLUE(.L,name): \
	.size name,GLUE(.L,name)-name
#endif

