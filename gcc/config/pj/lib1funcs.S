! lib1funcs.S for picoJava.
!   Copyright (C) 2000, 2001 Free Software Foundation, Inc.
! 
! This file is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by the
! Free Software Foundation; either version 2, or (at your option) any
! later version.
! 
! In addition to the permissions in the GNU General Public License, the
! Free Software Foundation gives you unlimited permission to link the
! compiled version of this file into combinations with other programs,
! and to distribute those combinations without any restriction coming
! from the use of this file.  (The General Public License restrictions
! do apply in other respects; for example, they cover modification of
! the file, and distribution when not linked into a combine
! executable.)
! 
! This file is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; see the file COPYING.  If not, write to
! the Free Software Foundation, 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.
! 


#ifdef Lvhelper

! The vhelper copies unnamed args in a varargs function from the
! opstack onto the aggregate stack.  It is a bit tricky since the
! opstack does not exist in real memory, so can not have its address taken,
! and since the opstack is being played with, there is nowhere to stick
! the temporaries.
	
	.globl __vhelper
__vhelper:


! incoming
! vars->  named0
!         named1
!         ...
!         unnamed0
!         unnamed1
!         ...
!         pc
!         vars
!         #named
!         return pc

	! work out total size everything below the named args and	
	! allocate that space on the aggregate stack + 3 extra words
	! for some temps.
	! g0   = old g0
	! g0+4 = vars
	! g0+8 = pc
	! g0+12 = last unnamed arg
	! ....

	write_global1
	write_global2

	! tos = #named args provided by callee.

	! move down the aggstack to make room for all the unnamed args
	! and the 12 bytes of extra stuff we have to pay attention to.
	! g0 = old_g0 - ((vars - optop) + named_bytes + 12) - stuff we just pushed

	! build new global0
	read_global0
 	 read_vars
	  read_optop
	 isub                ! tos = vars - optop (# bytes in all args)
          bipush 4
         isub                ! subtract out fudge for current stuff on stack.
 	  read_global2
	 isub                ! subtract out # words named.
        isub

	dup
	dup
	! store old global0 in new global0 spot.
	
	read_global0
        swap
	store_word

	! store new global0 value into global0
	write_global0

	! work out address to stop copying, which is vars - #named args bytes
	! but since we will have pushed stuff onto the stack when the comparison
	! is made, adjust by the fudge factor.
	 read_vars
 	  read_global2
	   bipush 12
	  iadd
 	 isub

	! optop= finish, vars, pc, ... 
	! now pop off args from the opstack and copy to aggstack till all done.
	! during the loop the opstack looks like
	! (optop_finish_addr) (destination_addr) (named_n) (named_n-1) ....
	! each iteration pops off one more element.


again:
	  dup_x2
	   read_optop
	 if_icmpeq done
	  iconst_4
	 iadd 
	  dup_x2
	store_word
	goto again

done:
	dup2_x1 ; pop2 ; pop   !leave pointer on top.

	! return to caller with varargs pointer as	
	! the next argument and the restoring global0 as the next. 

	read_global0 ; load_word

	! restore returning pc and vars
	read_global0 ; bipush 8; iadd; load_word
	read_global0 ; bipush 4; iadd; load_word

	! return to caller.
	read_global1
	write_pc
#endif


#ifdef __LITTLE_ENDIAN__
#define AL iload_1
#define AH iload_0
#define BL iload_3	
#define BH iload_2
#else
#define AL iload_0
#define AH iload_1
#define BL iload_2
#define BH iload_3
#endif	
#ifdef Lpjucmpdi2

! like ucmpdi2, but returns <0,0,>0 depending on comparison input.
! and returns answer on the stack, not in global1.  - much like an
! actual lucmp instruction would do if there was one.
!	      big         little
!  
!  vars-> 0 a low         high
!	  1 a high        low
!         2 b low         high
!	  3 b high        low
!
! compares a to b
! a > b return 1
! a = b return 0
! a < b return -1		
	
	.globl __pjucmpdi2
__pjucmpdi2:

! first see if we can compare the numbers using
! the signed instruction.
	
	AH
	BH
	if_icmpne	high_words_diff
	AL
	BL
	iucmp
	return1

! and low word if high word is equal.	

high_words_diff:		
	AH
	BH
	iucmp
	return1
#endif
