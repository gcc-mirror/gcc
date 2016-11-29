#undef __AAPCS64_BIG_ENDIAN__
#ifdef __GNUC__
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#define __AAPCS64_BIG_ENDIAN__
#endif
#else
#error unknown compiler
#endif

#define IN_FRAMEWORK

#define D0	0
#define D1	8
#define D2	16
#define D3	24
#define D4	32
#define D5	40
#define D6	48
#define D7	56

#define S0	64
#define S1	68
#define S2	72
#define S3	76
#define S4	80
#define S5	84
#define S6	88
#define S7	92

#define W0      96
#define W1     100
#define W2     104
#define W3     108
#define W4     112
#define W5     116
#define W6     120
#define W7     124

#define X0     128
#define X1     136
#define X2     144
#define X3     152
#define X4     160
#define X5     168
#define X6     176
#define X7     184

#define Q0     192
#define Q1     208
#define Q2     224
#define Q3     240
#define Q4     256
#define Q5     272
#define Q6     288
#define Q7     304

#define X8     320
#define X9     328

#define H0	336
#define H1	338
#define H2	340
#define H3	342
#define H4	344
#define H5	346
#define H6	348
#define H7	350


#define STACK  352

/* The type of test.  'myfunc' in abitest.S needs to know which kind of
   test it is running to decide what to do at the runtime.  Keep the
   related code in abitest.S synchronized if anything is changed here.  */
enum aapcs64_test_kind
{
  TK_PARAM = 0,	/* Test parameter passing.  */
  TK_VA_ARG,	/* Test va_arg code generation.  */
  TK_RETURN	/* Test function return value.  */
};

int which_kind_of_test;

extern int printf (const char*, ...);
extern void abort (void);
extern void dumpregs () __asm("myfunc");

#ifndef MYFUNCTYPE
#define MYFUNCTYPE void
#endif

#ifndef PCSATTR
#define PCSATTR
#endif


#ifdef RUNTIME_ENDIANNESS_CHECK
#ifndef RUNTIME_ENDIANNESS_CHECK_FUNCTION_DEFINED
/* This helper function defined to detect whether there is any incompatibility
   issue on endianness between compilation time and run-time environments.
   TODO: review the implementation when the work of big-endian support in A64
   GCC starts.
   */
static void rt_endian_check ()
{
  const char* msg_endian[2] = {"little-endian", "big-endian"};
  const char* msg_env[2] = {"compile-time", "run-time"};
  union
  {
    unsigned int ui;
    unsigned char ch[4];
  } u;
  int flag = -1;

  u.ui = 0xCAFEBABE;

  printf ("u.ui=0x%X, u.ch[0]=0x%X\n", u.ui, u.ch[0]);

  if (u.ch[0] == 0xBE)
    {
      /* Little-Endian at run-time */
#ifdef __AAPCS64_BIG_ENDIAN__
      /* Big-Endian at compile-time */
      flag = 1;
#endif
    }
  else
    {
      /* Big-Endian at run-time */
#ifndef __AAPCS64_BIG_ENDIAN__
      /* Little-Endian at compile-time */
      flag = 0;
#endif
    }

  if (flag != -1)
    {
      /* Endianness conflict exists */
      printf ("Error: endianness conflicts between %s and %s:\n\
\t%s: %s\n\t%s: %s\n", msg_env[0], msg_env[1], msg_env[0], msg_endian[flag],
		       msg_env[1], msg_endian[1-flag]);
      abort ();
    }

  return;
}
#endif
#define RUNTIME_ENDIANNESS_CHECK_FUNCTION_DEFINED
#endif
