/* dwarf2codeview.h - DWARF interface for CodeView generation.
   Copyright (C) 2023 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_DWARF2CODEVIEW_H
#define GCC_DWARF2CODEVIEW_H 1

#include "dwarf2out.h"
#include "flags.h"

/* Constants for in-built types.  */

#define T_VOID			0x0003
#define T_HRESULT		0x0008
#define T_CHAR			0x0010
#define T_SHORT			0x0011
#define T_LONG			0x0012
#define T_QUAD			0x0013
#define T_UCHAR			0x0020
#define T_USHORT		0x0021
#define T_ULONG			0x0022
#define T_UQUAD			0x0023
#define T_BOOL08		0x0030
#define T_REAL32		0x0040
#define T_REAL64		0x0041
#define T_REAL80		0x0042
#define T_REAL128		0x0043
#define T_RCHAR			0x0070
#define T_WCHAR			0x0071
#define T_INT4			0x0074
#define T_UINT4			0x0075
#define T_CHAR16		0x007a
#define T_CHAR32		0x007b
#define T_CHAR8			0x007c

#define CV_POINTER_32		0x0400
#define CV_POINTER_64		0x0600
#define T_32PVOID		(T_VOID | CV_POINTER_32)
#define T_64PVOID		(T_VOID | CV_POINTER_64)

/* LF_POINTER attributes.  */
#define CV_PTR_NEAR32		0x0a
#define CV_PTR_64		0x0c

/* enum CV_ptrmode_e in cvinfo.h, shifted by 5 for the lfPointerAttr bitfield */
#define CV_PTR_MODE_MASK	0xe0
#define CV_PTR_MODE_LVREF	0x20
#define CV_PTR_MODE_PMEM	0x40
#define CV_PTR_MODE_PMFUNC	0x60
#define CV_PTR_MODE_RVREF	0x80

/* enum CV_pmtype_e in in cvinfo.h */
#define CV_PMTYPE_D_Single	0x01
#define CV_PMTYPE_F_Single	0x05

/* LF_MODIFIER values.  */
#define MOD_const		0x1
#define MOD_volatile		0x2

#define CV_ACCESS_PRIVATE	1
#define CV_ACCESS_PROTECTED	2
#define CV_ACCESS_PUBLIC	3

/* CV_methodprop_e values in cvinfo.h, shifted by 2 for CV_fldattr_t.  */
#define CV_METHOD_VANILLA	0x00
#define CV_METHOD_VIRTUAL	0x04
#define CV_METHOD_STATIC	0x08
#define CV_METHOD_FRIEND	0x0c
#define CV_METHOD_INTRO		0x10
#define CV_METHOD_PUREVIRT	0x14
#define CV_METHOD_PUREINTRO	0x18

#define CV_PROP_FWDREF		0x80

/* Debug Format Interface.  Used in dwarf2out.cc.  */

extern void codeview_debug_finish (void);
extern void codeview_source_line (unsigned int, const char *);
extern void codeview_start_source_file (const char *);
extern void codeview_switch_text_section ();
extern void codeview_end_epilogue (void);
extern void codeview_debug_early_finish (dw_die_ref die);
extern void codeview_begin_block (unsigned int, unsigned int, tree);
extern void codeview_end_block (unsigned int, unsigned int);
extern void codeview_abstract_function (tree);

#endif /* GCC_DWARF2CODEVIEW_H */
