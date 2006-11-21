/* Copyright (C) 2006 Free Software Foundation, Inc.
  
   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 2 of the License, or (at your option)
   any later version.
  
   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.
  
   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.  */

/* As a special exception, if you link this library with files compiled with
   GCC to produce an executable, this does not cause the resulting executable
   to be covered by the GNU General Public License.  The exception does not
   however invalidate any other reasons why the executable file might be covered
   by the GNU General Public License. */

extern int main(int, unsigned long long, unsigned long long);
void _start(int, unsigned long long, unsigned long long) __attribute__((__naked__));

extern void exit(int);

void _exit(int) __attribute__((__naked__));

typedef void (*func_ptr) (void);
typedef __attribute__ ((__vector_size__(16))) unsigned int vec_uint4;

extern vec_uint4 __stack[];
register vec_uint4 si_sp __asm__("$sp");
register unsigned int si_r2 __asm__("$2");

extern char _end[];

/* If we want these aligned we need to do it in the linker script. */
func_ptr __CTOR_LIST__[1]
  __attribute__ ((__section__(".ctors"), __aligned__(4)))
  = { (func_ptr) (-1) };

static func_ptr __DTOR_LIST__[1]
  __attribute__((__section__(".dtors"), __aligned__(4)))
  = { (func_ptr) (-1) };


/* According to the BE Linux ABI an SPU module is called with these
 * parameters.  Also, $2 is set to the Available Stack Size.  */
void
_start(int spu_id,
       unsigned long long param,
       unsigned long long env)
{
  unsigned int stack_size;
  unsigned int sp = (unsigned int)(__stack - 2);

  /* Initialize the stack.  __stack has been set to point to the top
     quadword of the stack.  The ABI requires at least a NULL terminated
     back chain and lr save area.  For example:
         +----------------+
	 | 0              |
         +----------------+  <-  __stack (e.g., 0x3fff0)
	 | space for $lr  |
         +----------------+
	 | back chain     |
         +----------------+  <-  $sp  (e.g., __stack - 32, 0x3ffd0)
  */
  __stack[0] = (vec_uint4){0, 0, 0, 0};
  __stack[-1] = (vec_uint4){0, 0, 0, 0};

  /* Initialize the Available Stack Size word of the Stack Pointer
   * information register.  The BE Linux ABI passes the stack size in
   * $2, or use everything up to _end if $2 == 0. */
  stack_size = si_r2 == 0 ? sp - (unsigned int)_end : si_r2;

  __stack[-2] = (vec_uint4){(unsigned int)__stack, stack_size, 0, 0};

  si_sp = (vec_uint4){sp, stack_size, 0, 0};


  {
    extern func_ptr __CTOR_END__[];
    func_ptr *p;

    /* The compiler assumes all symbols are 16 byte aligned, which is
     * not the case for __CTOR_END__.  This inline assembly makes sure
     * the address is loaded into a register for which the compiler does
     * not assume anything about alignment. */
    __asm__ ("\n" : "=r" (p) : "0" (__CTOR_END__ - 1));

    for (; *p != (func_ptr) -1; p--)
      (*p) ();
  }

  exit(main(spu_id, param, env));
  __asm__ volatile ( "	stop	0x20ff");
}

/* C99 requires _Exit */
void _Exit(int) __attribute__((__weak__, __alias__("_exit")));

void
_exit(int rc)
{
  {
    static func_ptr *p = 0;
    if (!p)
      {
	/* See comment for __CTOR_END__ above. */
	__asm__ ("" : "=r" (p) : "0" (__DTOR_LIST__ + 1));
	for (; *p; p++)
	  (*p) ();
      }
  }
  /* Some self modifying code to return 'rc' in the 'stop' insn. */
  __asm__ volatile (
    "	ori	$3, %0,0\n"
    "	lqr	$4, 1f\n"
    "	cbd	$5, 1f+3($sp)\n"
    "	shufb	$0, %0, $4, $5\n"
    "	stqr	$0, 1f\n"
    "	sync\n"
    "1:\n"
    "	stop	0x2000\n"
    : : "r" (rc) );
}

