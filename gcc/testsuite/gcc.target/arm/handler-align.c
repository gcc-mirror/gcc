/* Test epilogue of a realigned interrupt handler. */
/* { dg-do run } */
/* { dg-options "-mthumb -Os" } */
/* { dg-skip-if "" { ! { arm_thumb1_ok || arm_thumb2_ok } } } */
/* { dg-require-effective-target arm_cortex_m } */
/* { dg-require-effective-target arm_eabi } */

extern __attribute__((noreturn)) void abort(void);
extern int snprintf(char *, int, const char *, ...);

#define BUFF_LEN 256
char buff[BUFF_LEN];

char *get_buffer(void)
{
	return buff;
}

void __attribute__((interrupt)) foo(void)
{
        char *msg = get_buffer();
        snprintf(msg, BUFF_LEN, "%d %p", 1, buff+BUFF_LEN);
}

volatile void * save_sp;
int main()
{
	register volatile void * sp asm("sp");
	/* Check stack pointer before/after calling the interrupt
         * handler. Not equal means that handler doesn't restore
         * stack correctly.  */
	save_sp = sp;
	foo();
	/* Abort here instead of return non-zero. Due to wrong sp, lr value,
	 * returning from main may not work.  */
	if (save_sp != sp)
	{
		sp = save_sp;
		abort();
	}
	return 0;
}
