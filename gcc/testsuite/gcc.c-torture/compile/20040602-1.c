/* Test type qualifiers.  These should as equal types.  */
extern volatile unsigned long foo;
typedef unsigned long ulong;
extern volatile ulong foo;
volatile ulong foo;
