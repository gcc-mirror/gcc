# crti.s for eabi

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
 
	.file	"crti.s"
	.ident	"GNU C crti.s"

	.section ".got","w"
	.globl	__GOT_START__
	.type	__GOT_START__,@object
	.weak	_GLOBAL_OFFSET_TABLE_
	.type	_GLOBAL_OFFSET_TABLE_,@object
	.weak	_SDA_BASE_
	.type	_SDA_BASE_,@object
__GOT_START__:
_GLOBAL_OFFSET_TABLE_:
_SDA_BASE_:

	.section ".got1","w"
	.globl	__GOT1_START__
	.type	__GOT1_START__,@object
__GOT1_START__:

	.section ".got2","w"
	.globl	__GOT2_START__
	.type	__GOT2_START__,@object
__GOT2_START__:

	.section ".fixup","w"
	.globl	__FIXUP_START__
	.type	__FIXUP_START__,@object
__FIXUP_START__:

	.section ".ctors","w"
	.globl	__CTOR_LIST__
	.type	__CTOR_LIST__,@object
__CTOR_LIST__:

	.section ".dtors","w"
	.globl	__CTOR_LIST__
	.type	__CTOR_LIST__,@object
__DTOR_LIST__:
