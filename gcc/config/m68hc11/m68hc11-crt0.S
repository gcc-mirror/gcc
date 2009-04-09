/* Startup code for M68HC11.
   Copyright (C) 1999, 2000, 2002, 2008, 2009 Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */
	
;-----------------------------------------
; startup code
;-----------------------------------------

#ifdef __HAVE_SHORT_INT__
	.mode mshort
#else
	.mode mlong
#endif

#if defined(__USE_RTC__) && defined(mc68hc12)
	.macro jsr name
	call	\name
	.endm
#endif
;; 
;; 
;; The linker concatenate the .install* sections in the following order:
;; 
;; .install0	Setup the stack pointer
;; .install1	Place holder for applications
;; .install2	Optional installation of data section in memory
;; .install3	Place holder for applications
;; .install4	Invokes the main
;; 
	.sect   .install0,"ax",@progbits
	.globl _start

_start:
;;
;; At this step, the stack is not initialized and interrupts are masked.
;; Applications only have 64 cycles to initialize some registers.
;;
;; To have a generic/configurable startup, initialize the stack to
;; the end of some memory region.  The _stack symbol is defined by
;; the linker.
;;
	lds	#_stack
	
	.sect	.install2,"ax",@progbits
;;
;; Call a specific initialization operation.  The default is empty.
;; It can be overridden by applications.  It is intended to initialize
;; the 68hc11 registers.  Function prototype is:
;; 
;;	int __premain(void);
;; 
	jsr	__premain
	
;;
;; 
;;
	.sect	.install4,"ax",@progbits
	jsr     main
fatal:
	jsr	exit
	bra fatal

;-----------------------------------------
; end startup code
;-----------------------------------------
;; Force loading of data section mapping and bss clear
	.2byte	__map_data_section
	.2byte  __init_bss_section
