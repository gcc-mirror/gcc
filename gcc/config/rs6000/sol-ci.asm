# crti.s for solaris

#   Copyright (C) 1996 Free Software Foundation, Inc.
#   Written By Michael Meissner
# 
# This file is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
# 
# In addition to the permissions in the GNU General Public License, the
# Free Software Foundation gives you unlimited permission to link the
# compiled version of this file with other programs, and to distribute
# those programs without any restriction coming from the use of this
# file.  (The General Public License restrictions do apply in other
# respects; for example, they cover modification of the file, and
# distribution when not linked into another program.)
# 
# This file is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING.  If not, write to
# the Free Software Foundation, 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.
# 
#    As a special exception, if you link this library with files
#    compiled with GCC to produce an executable, this does not cause
#    the resulting executable to be covered by the GNU General Public License.
#    This exception does not however invalidate any other reasons why
#    the executable file might be covered by the GNU General Public License.
# 

# This file just supplies labeled starting points for the .got* and other
# special sections.  It is linked in first before other modules.
 
	.file	"scrti.s"
	.ident	"GNU C scrti.s"

# List of C++ constructors
	.section ".ctors","aw"
	.globl	__CTOR_LIST__
	.type	__CTOR_LIST__,@object
__CTOR_LIST__:

# List of C++ destructors
	.section ".dtors","aw"
	.globl	__DTOR_LIST__
	.type	__DTOR_LIST__,@object
__DTOR_LIST__:

# Head of __init function used for static constructors in Solaris
	.section ".init","ax"
	.align 2
	.globl __init
	.type __init,@function
__init:	stwu %r1,-16(%r1)
	mflr %r0
	stw %r0,12(%r1)

# Head of __fini function used for static destructors in Solaris
	.section ".fini","ax"
	.align 2
	.globl __fini
	.type __fini,@function
__fini:	stwu %r1,-16(%r1)
	mflr %r0
	stw %r0,12(%r1)
