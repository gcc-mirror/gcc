/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-mgeneral-regs-only" } */

extern void exit (int);

typedef unsigned int uword_t __attribute__ ((mode (__word__)));

#define ERROR		0x12345670
#define IP		0x12345671
#define CS		0x12345672
#define FLAGS		0x12345673
#define SP		0x12345674
#define SS		0x12345675

#define STRING(x)	XSTRING(x)
#define XSTRING(x)	#x

struct interrupt_frame
{
  uword_t ip;
  uword_t cs;
  uword_t flags;
  uword_t sp;
  uword_t ss;
};

__attribute__((interrupt, used))
void
fn (struct interrupt_frame *frame, uword_t error)
{
  if (ERROR != error)
    __builtin_abort ();
  if (IP != frame->ip)
    __builtin_abort ();
  if (CS != frame->cs)
    __builtin_abort ();
  if (FLAGS != frame->flags)
    __builtin_abort ();
  if (SP != frame->sp)
    __builtin_abort ();
  if (SS != frame->ss)
    __builtin_abort ();

  exit (0);
}

int
main ()
{
  asm ("push	$" STRING (SS) ";		\
	push	$" STRING (SP) ";		\
	push	$" STRING (FLAGS) ";		\
	push	$" STRING (CS) ";		\
	push	$" STRING (IP) ";		\
	push	$" STRING (ERROR) ";		\
	jmp	fn");
  return 0;
}
