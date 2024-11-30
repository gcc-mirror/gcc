/* Generate CodeView debugging info from the GCC DWARF.
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

/* See gas/codeview.h in binutils for more about the constants and structs
   listed below.  References to Microsoft files refer to Microsoft's PDB
   repository: https://github.com/microsoft/microsoft-pdb.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "output.h"
#include "errors.h"
#include "md5.h"
#include "function.h"
#include "version.h"
#include "tree.h"
#include "langhooks.h"
#include "dwarf2out.h"
#include "dwarf2codeview.h"
#include "rtl.h"

#ifdef CODEVIEW_DEBUGGING_INFO

#define CV_SIGNATURE_C13	4

#define DEBUG_S_SYMBOLS		0xf1
#define DEBUG_S_LINES		0xf2
#define DEBUG_S_STRINGTABLE     0xf3
#define DEBUG_S_FILECHKSMS      0xf4
#define DEBUG_S_INLINEELINES	0xf6

#define CHKSUM_TYPE_MD5		1

#define CV_CFL_80386		0x03
#define CV_CFL_X64		0xD0

#define CV_CFL_C		0x00
#define CV_CFL_CXX		0x01

#define CV_INLINEE_SOURCE_LINE_SIGNATURE	0x0

#define FIRST_TYPE		0x1000

#define LINE_LABEL	"Lcvline"
#define END_FUNC_LABEL	"Lcvendfunc"
#define SYMBOL_START_LABEL	"Lcvsymstart"
#define SYMBOL_END_LABEL	"Lcvsymend"

/* There's two bytes available for each type's size, but follow MSVC's lead in
   capping the LF_FIELDLIST size at fb00 (minus 8 bytes for the LF_INDEX
   pointing to the overflow entry).  */
#define MAX_FIELDLIST_SIZE	0xfaf8

#define HASH_SIZE 16

/* This is enum SYM_ENUM_e in Microsoft's cvinfo.h.  */

enum cv_sym_type {
  S_END = 0x0006,
  S_FRAMEPROC = 0x1012,
  S_BLOCK32 = 0x1103,
  S_REGISTER = 0x1106,
  S_LDATA32 = 0x110c,
  S_GDATA32 = 0x110d,
  S_REGREL32 = 0x1111,
  S_COMPILE3 = 0x113c,
  S_LOCAL = 0x113e,
  S_DEFRANGE_REGISTER = 0x1141,
  S_DEFRANGE_REGISTER_REL = 0x1145,
  S_LPROC32_ID = 0x1146,
  S_GPROC32_ID = 0x1147,
  S_INLINESITE = 0x114d,
  S_INLINESITE_END = 0x114e,
  S_PROC_ID_END = 0x114f
};

/* This is enum LEAF_ENUM_e in Microsoft's cvinfo.h.  */

enum cv_leaf_type {
  LF_PAD1 = 0xf1,
  LF_PAD2 = 0xf2,
  LF_PAD3 = 0xf3,
  LF_MODIFIER = 0x1001,
  LF_POINTER = 0x1002,
  LF_PROCEDURE = 0x1008,
  LF_MFUNCTION = 0x1009,
  LF_ARGLIST = 0x1201,
  LF_FIELDLIST = 0x1203,
  LF_BITFIELD = 0x1205,
  LF_METHODLIST = 0x1206,
  LF_BCLASS = 0x1400,
  LF_INDEX = 0x1404,
  LF_ENUMERATE = 0x1502,
  LF_ARRAY = 0x1503,
  LF_CLASS = 0x1504,
  LF_STRUCTURE = 0x1505,
  LF_UNION = 0x1506,
  LF_ENUM = 0x1507,
  LF_MEMBER = 0x150d,
  LF_STMEMBER = 0x150e,
  LF_METHOD = 0x150f,
  LF_NESTTYPE = 0x1510,
  LF_ONEMETHOD = 0x1511,
  LF_FUNC_ID = 0x1601,
  LF_MFUNC_ID = 0x1602,
  LF_STRING_ID = 0x1605,
  LF_CHAR = 0x8000,
  LF_SHORT = 0x8001,
  LF_USHORT = 0x8002,
  LF_LONG = 0x8003,
  LF_ULONG = 0x8004,
  LF_QUADWORD = 0x8009,
  LF_UQUADWORD = 0x800a
};

/* These come from enum CV_HREG_e in Microsoft's cvconst.h.  */

enum cv_x86_register {
  CV_REG_NONE = 0,
  CV_REG_AL = 1,
  CV_REG_CL = 2,
  CV_REG_DL = 3,
  CV_REG_BL = 4,
  CV_REG_AH = 5,
  CV_REG_CH = 6,
  CV_REG_DH = 7,
  CV_REG_BH = 8,
  CV_REG_AX = 9,
  CV_REG_CX = 10,
  CV_REG_DX = 11,
  CV_REG_BX = 12,
  CV_REG_SP = 13,
  CV_REG_BP = 14,
  CV_REG_SI = 15,
  CV_REG_DI = 16,
  CV_REG_EAX = 17,
  CV_REG_ECX = 18,
  CV_REG_EDX = 19,
  CV_REG_EBX = 20,
  CV_REG_ESP = 21,
  CV_REG_EBP = 22,
  CV_REG_ESI = 23,
  CV_REG_EDI = 24,
  CV_REG_ES = 25,
  CV_REG_CS = 26,
  CV_REG_SS = 27,
  CV_REG_DS = 28,
  CV_REG_FS = 29,
  CV_REG_GS = 30,
  CV_REG_IP = 31,
  CV_REG_FLAGS = 32,
  CV_REG_EIP = 33,
  CV_REG_EFLAGS = 34,
  CV_REG_TEMP = 40,
  CV_REG_TEMPH = 41,
  CV_REG_QUOTE = 42,
  CV_REG_PCDR3 = 43,
  CV_REG_PCDR4 = 44,
  CV_REG_PCDR5 = 45,
  CV_REG_PCDR6 = 46,
  CV_REG_PCDR7 = 47,
  CV_REG_CR0 = 80,
  CV_REG_CR1 = 81,
  CV_REG_CR2 = 82,
  CV_REG_CR3 = 83,
  CV_REG_CR4 = 84,
  CV_REG_DR0 = 90,
  CV_REG_DR1 = 91,
  CV_REG_DR2 = 92,
  CV_REG_DR3 = 93,
  CV_REG_DR4 = 94,
  CV_REG_DR5 = 95,
  CV_REG_DR6 = 96,
  CV_REG_DR7 = 97,
  CV_REG_GDTR = 110,
  CV_REG_GDTL = 111,
  CV_REG_IDTR = 112,
  CV_REG_IDTL = 113,
  CV_REG_LDTR = 114,
  CV_REG_TR = 115,
  CV_REG_PSEUDO1 = 116,
  CV_REG_PSEUDO2 = 117,
  CV_REG_PSEUDO3 = 118,
  CV_REG_PSEUDO4 = 119,
  CV_REG_PSEUDO5 = 120,
  CV_REG_PSEUDO6 = 121,
  CV_REG_PSEUDO7 = 122,
  CV_REG_PSEUDO8 = 123,
  CV_REG_PSEUDO9 = 124,
  CV_REG_ST0 = 128,
  CV_REG_ST1 = 129,
  CV_REG_ST2 = 130,
  CV_REG_ST3 = 131,
  CV_REG_ST4 = 132,
  CV_REG_ST5 = 133,
  CV_REG_ST6 = 134,
  CV_REG_ST7 = 135,
  CV_REG_CTRL = 136,
  CV_REG_STAT = 137,
  CV_REG_TAG = 138,
  CV_REG_FPIP = 139,
  CV_REG_FPCS = 140,
  CV_REG_FPDO = 141,
  CV_REG_FPDS = 142,
  CV_REG_ISEM = 143,
  CV_REG_FPEIP = 144,
  CV_REG_FPEDO = 145,
  CV_REG_MM0 = 146,
  CV_REG_MM1 = 147,
  CV_REG_MM2 = 148,
  CV_REG_MM3 = 149,
  CV_REG_MM4 = 150,
  CV_REG_MM5 = 151,
  CV_REG_MM6 = 152,
  CV_REG_MM7 = 153,
  CV_REG_XMM0 = 154,
  CV_REG_XMM1 = 155,
  CV_REG_XMM2 = 156,
  CV_REG_XMM3 = 157,
  CV_REG_XMM4 = 158,
  CV_REG_XMM5 = 159,
  CV_REG_XMM6 = 160,
  CV_REG_XMM7 = 161,
  CV_REG_XMM00 = 162,
  CV_REG_XMM01 = 163,
  CV_REG_XMM02 = 164,
  CV_REG_XMM03 = 165,
  CV_REG_XMM10 = 166,
  CV_REG_XMM11 = 167,
  CV_REG_XMM12 = 168,
  CV_REG_XMM13 = 169,
  CV_REG_XMM20 = 170,
  CV_REG_XMM21 = 171,
  CV_REG_XMM22 = 172,
  CV_REG_XMM23 = 173,
  CV_REG_XMM30 = 174,
  CV_REG_XMM31 = 175,
  CV_REG_XMM32 = 176,
  CV_REG_XMM33 = 177,
  CV_REG_XMM40 = 178,
  CV_REG_XMM41 = 179,
  CV_REG_XMM42 = 180,
  CV_REG_XMM43 = 181,
  CV_REG_XMM50 = 182,
  CV_REG_XMM51 = 183,
  CV_REG_XMM52 = 184,
  CV_REG_XMM53 = 185,
  CV_REG_XMM60 = 186,
  CV_REG_XMM61 = 187,
  CV_REG_XMM62 = 188,
  CV_REG_XMM63 = 189,
  CV_REG_XMM70 = 190,
  CV_REG_XMM71 = 191,
  CV_REG_XMM72 = 192,
  CV_REG_XMM73 = 193,
  CV_REG_XMM0L = 194,
  CV_REG_XMM1L = 195,
  CV_REG_XMM2L = 196,
  CV_REG_XMM3L = 197,
  CV_REG_XMM4L = 198,
  CV_REG_XMM5L = 199,
  CV_REG_XMM6L = 200,
  CV_REG_XMM7L = 201,
  CV_REG_XMM0H = 202,
  CV_REG_XMM1H = 203,
  CV_REG_XMM2H = 204,
  CV_REG_XMM3H = 205,
  CV_REG_XMM4H = 206,
  CV_REG_XMM5H = 207,
  CV_REG_XMM6H = 208,
  CV_REG_XMM7H = 209,
  CV_REG_MXCSR = 211,
  CV_REG_EDXEAX = 212,
  CV_REG_EMM0L = 220,
  CV_REG_EMM1L = 221,
  CV_REG_EMM2L = 222,
  CV_REG_EMM3L = 223,
  CV_REG_EMM4L = 224,
  CV_REG_EMM5L = 225,
  CV_REG_EMM6L = 226,
  CV_REG_EMM7L = 227,
  CV_REG_EMM0H = 228,
  CV_REG_EMM1H = 229,
  CV_REG_EMM2H = 230,
  CV_REG_EMM3H = 231,
  CV_REG_EMM4H = 232,
  CV_REG_EMM5H = 233,
  CV_REG_EMM6H = 234,
  CV_REG_EMM7H = 235,
  CV_REG_MM00 = 236,
  CV_REG_MM01 = 237,
  CV_REG_MM10 = 238,
  CV_REG_MM11 = 239,
  CV_REG_MM20 = 240,
  CV_REG_MM21 = 241,
  CV_REG_MM30 = 242,
  CV_REG_MM31 = 243,
  CV_REG_MM40 = 244,
  CV_REG_MM41 = 245,
  CV_REG_MM50 = 246,
  CV_REG_MM51 = 247,
  CV_REG_MM60 = 248,
  CV_REG_MM61 = 249,
  CV_REG_MM70 = 250,
  CV_REG_MM71 = 251,
  CV_REG_YMM0 = 252,
  CV_REG_YMM1 = 253,
  CV_REG_YMM2 = 254,
  CV_REG_YMM3 = 255,
  CV_REG_YMM4 = 256,
  CV_REG_YMM5 = 257,
  CV_REG_YMM6 = 258,
  CV_REG_YMM7 = 259,
  CV_REG_YMM0H = 260,
  CV_REG_YMM1H = 261,
  CV_REG_YMM2H = 262,
  CV_REG_YMM3H = 263,
  CV_REG_YMM4H = 264,
  CV_REG_YMM5H = 265,
  CV_REG_YMM6H = 266,
  CV_REG_YMM7H = 267,
  CV_REG_YMM0I0 = 268,
  CV_REG_YMM0I1 = 269,
  CV_REG_YMM0I2 = 270,
  CV_REG_YMM0I3 = 271,
  CV_REG_YMM1I0 = 272,
  CV_REG_YMM1I1 = 273,
  CV_REG_YMM1I2 = 274,
  CV_REG_YMM1I3 = 275,
  CV_REG_YMM2I0 = 276,
  CV_REG_YMM2I1 = 277,
  CV_REG_YMM2I2 = 278,
  CV_REG_YMM2I3 = 279,
  CV_REG_YMM3I0 = 280,
  CV_REG_YMM3I1 = 281,
  CV_REG_YMM3I2 = 282,
  CV_REG_YMM3I3 = 283,
  CV_REG_YMM4I0 = 284,
  CV_REG_YMM4I1 = 285,
  CV_REG_YMM4I2 = 286,
  CV_REG_YMM4I3 = 287,
  CV_REG_YMM5I0 = 288,
  CV_REG_YMM5I1 = 289,
  CV_REG_YMM5I2 = 290,
  CV_REG_YMM5I3 = 291,
  CV_REG_YMM6I0 = 292,
  CV_REG_YMM6I1 = 293,
  CV_REG_YMM6I2 = 294,
  CV_REG_YMM6I3 = 295,
  CV_REG_YMM7I0 = 296,
  CV_REG_YMM7I1 = 297,
  CV_REG_YMM7I2 = 298,
  CV_REG_YMM7I3 = 299,
  CV_REG_YMM0F0 = 300,
  CV_REG_YMM0F1 = 301,
  CV_REG_YMM0F2 = 302,
  CV_REG_YMM0F3 = 303,
  CV_REG_YMM0F4 = 304,
  CV_REG_YMM0F5 = 305,
  CV_REG_YMM0F6 = 306,
  CV_REG_YMM0F7 = 307,
  CV_REG_YMM1F0 = 308,
  CV_REG_YMM1F1 = 309,
  CV_REG_YMM1F2 = 310,
  CV_REG_YMM1F3 = 311,
  CV_REG_YMM1F4 = 312,
  CV_REG_YMM1F5 = 313,
  CV_REG_YMM1F6 = 314,
  CV_REG_YMM1F7 = 315,
  CV_REG_YMM2F0 = 316,
  CV_REG_YMM2F1 = 317,
  CV_REG_YMM2F2 = 318,
  CV_REG_YMM2F3 = 319,
  CV_REG_YMM2F4 = 320,
  CV_REG_YMM2F5 = 321,
  CV_REG_YMM2F6 = 322,
  CV_REG_YMM2F7 = 323,
  CV_REG_YMM3F0 = 324,
  CV_REG_YMM3F1 = 325,
  CV_REG_YMM3F2 = 326,
  CV_REG_YMM3F3 = 327,
  CV_REG_YMM3F4 = 328,
  CV_REG_YMM3F5 = 329,
  CV_REG_YMM3F6 = 330,
  CV_REG_YMM3F7 = 331,
  CV_REG_YMM4F0 = 332,
  CV_REG_YMM4F1 = 333,
  CV_REG_YMM4F2 = 334,
  CV_REG_YMM4F3 = 335,
  CV_REG_YMM4F4 = 336,
  CV_REG_YMM4F5 = 337,
  CV_REG_YMM4F6 = 338,
  CV_REG_YMM4F7 = 339,
  CV_REG_YMM5F0 = 340,
  CV_REG_YMM5F1 = 341,
  CV_REG_YMM5F2 = 342,
  CV_REG_YMM5F3 = 343,
  CV_REG_YMM5F4 = 344,
  CV_REG_YMM5F5 = 345,
  CV_REG_YMM5F6 = 346,
  CV_REG_YMM5F7 = 347,
  CV_REG_YMM6F0 = 348,
  CV_REG_YMM6F1 = 349,
  CV_REG_YMM6F2 = 350,
  CV_REG_YMM6F3 = 351,
  CV_REG_YMM6F4 = 352,
  CV_REG_YMM6F5 = 353,
  CV_REG_YMM6F6 = 354,
  CV_REG_YMM6F7 = 355,
  CV_REG_YMM7F0 = 356,
  CV_REG_YMM7F1 = 357,
  CV_REG_YMM7F2 = 358,
  CV_REG_YMM7F3 = 359,
  CV_REG_YMM7F4 = 360,
  CV_REG_YMM7F5 = 361,
  CV_REG_YMM7F6 = 362,
  CV_REG_YMM7F7 = 363,
  CV_REG_YMM0D0 = 364,
  CV_REG_YMM0D1 = 365,
  CV_REG_YMM0D2 = 366,
  CV_REG_YMM0D3 = 367,
  CV_REG_YMM1D0 = 368,
  CV_REG_YMM1D1 = 369,
  CV_REG_YMM1D2 = 370,
  CV_REG_YMM1D3 = 371,
  CV_REG_YMM2D0 = 372,
  CV_REG_YMM2D1 = 373,
  CV_REG_YMM2D2 = 374,
  CV_REG_YMM2D3 = 375,
  CV_REG_YMM3D0 = 376,
  CV_REG_YMM3D1 = 377,
  CV_REG_YMM3D2 = 378,
  CV_REG_YMM3D3 = 379,
  CV_REG_YMM4D0 = 380,
  CV_REG_YMM4D1 = 381,
  CV_REG_YMM4D2 = 382,
  CV_REG_YMM4D3 = 383,
  CV_REG_YMM5D0 = 384,
  CV_REG_YMM5D1 = 385,
  CV_REG_YMM5D2 = 386,
  CV_REG_YMM5D3 = 387,
  CV_REG_YMM6D0 = 388,
  CV_REG_YMM6D1 = 389,
  CV_REG_YMM6D2 = 390,
  CV_REG_YMM6D3 = 391,
  CV_REG_YMM7D0 = 392,
  CV_REG_YMM7D1 = 393,
  CV_REG_YMM7D2 = 394,
  CV_REG_YMM7D3 = 395,
  CV_REG_BND0 = 396,
  CV_REG_BND1 = 397,
  CV_REG_BND2 = 398,
  CV_REG_BND3 = 399
};

enum cv_amd64_register {
  CV_AMD64_NOREG = 0,
  CV_AMD64_AL = 1,
  CV_AMD64_CL = 2,
  CV_AMD64_DL = 3,
  CV_AMD64_BL = 4,
  CV_AMD64_AH = 5,
  CV_AMD64_CH = 6,
  CV_AMD64_DH = 7,
  CV_AMD64_BH = 8,
  CV_AMD64_AX = 9,
  CV_AMD64_CX = 10,
  CV_AMD64_DX = 11,
  CV_AMD64_BX = 12,
  CV_AMD64_SP = 13,
  CV_AMD64_BP = 14,
  CV_AMD64_SI = 15,
  CV_AMD64_DI = 16,
  CV_AMD64_EAX = 17,
  CV_AMD64_ECX = 18,
  CV_AMD64_EDX = 19,
  CV_AMD64_EBX = 20,
  CV_AMD64_ESP = 21,
  CV_AMD64_EBP = 22,
  CV_AMD64_ESI = 23,
  CV_AMD64_EDI = 24,
  CV_AMD64_ES = 25,
  CV_AMD64_CS = 26,
  CV_AMD64_SS = 27,
  CV_AMD64_DS = 28,
  CV_AMD64_FS = 29,
  CV_AMD64_GS = 30,
  CV_AMD64_FLAGS = 32,
  CV_AMD64_RIP = 33,
  CV_AMD64_EFLAGS = 34,
  CV_AMD64_CR0 = 80,
  CV_AMD64_CR1 = 81,
  CV_AMD64_CR2 = 82,
  CV_AMD64_CR3 = 83,
  CV_AMD64_CR4 = 84,
  CV_AMD64_CR8 = 88,
  CV_AMD64_DR0 = 90,
  CV_AMD64_DR1 = 91,
  CV_AMD64_DR2 = 92,
  CV_AMD64_DR3 = 93,
  CV_AMD64_DR4 = 94,
  CV_AMD64_DR5 = 95,
  CV_AMD64_DR6 = 96,
  CV_AMD64_DR7 = 97,
  CV_AMD64_DR8 = 98,
  CV_AMD64_DR9 = 99,
  CV_AMD64_DR10 = 100,
  CV_AMD64_DR11 = 101,
  CV_AMD64_DR12 = 102,
  CV_AMD64_DR13 = 103,
  CV_AMD64_DR14 = 104,
  CV_AMD64_DR15 = 105,
  CV_AMD64_GDTR = 110,
  CV_AMD64_GDTL = 111,
  CV_AMD64_IDTR = 112,
  CV_AMD64_IDTL = 113,
  CV_AMD64_LDTR = 114,
  CV_AMD64_TR = 115,
  CV_AMD64_ST0 = 128,
  CV_AMD64_ST1 = 129,
  CV_AMD64_ST2 = 130,
  CV_AMD64_ST3 = 131,
  CV_AMD64_ST4 = 132,
  CV_AMD64_ST5 = 133,
  CV_AMD64_ST6 = 134,
  CV_AMD64_ST7 = 135,
  CV_AMD64_CTRL = 136,
  CV_AMD64_STAT = 137,
  CV_AMD64_TAG = 138,
  CV_AMD64_FPIP = 139,
  CV_AMD64_FPCS = 140,
  CV_AMD64_FPDO = 141,
  CV_AMD64_FPDS = 142,
  CV_AMD64_ISEM = 143,
  CV_AMD64_FPEIP = 144,
  CV_AMD64_FPEDO = 145,
  CV_AMD64_MM0 = 146,
  CV_AMD64_MM1 = 147,
  CV_AMD64_MM2 = 148,
  CV_AMD64_MM3 = 149,
  CV_AMD64_MM4 = 150,
  CV_AMD64_MM5 = 151,
  CV_AMD64_MM6 = 152,
  CV_AMD64_MM7 = 153,
  CV_AMD64_XMM0 = 154,
  CV_AMD64_XMM1 = 155,
  CV_AMD64_XMM2 = 156,
  CV_AMD64_XMM3 = 157,
  CV_AMD64_XMM4 = 158,
  CV_AMD64_XMM5 = 159,
  CV_AMD64_XMM6 = 160,
  CV_AMD64_XMM7 = 161,
  CV_AMD64_XMM0_0 = 162,
  CV_AMD64_XMM0_1 = 163,
  CV_AMD64_XMM0_2 = 164,
  CV_AMD64_XMM0_3 = 165,
  CV_AMD64_XMM1_0 = 166,
  CV_AMD64_XMM1_1 = 167,
  CV_AMD64_XMM1_2 = 168,
  CV_AMD64_XMM1_3 = 169,
  CV_AMD64_XMM2_0 = 170,
  CV_AMD64_XMM2_1 = 171,
  CV_AMD64_XMM2_2 = 172,
  CV_AMD64_XMM2_3 = 173,
  CV_AMD64_XMM3_0 = 174,
  CV_AMD64_XMM3_1 = 175,
  CV_AMD64_XMM3_2 = 176,
  CV_AMD64_XMM3_3 = 177,
  CV_AMD64_XMM4_0 = 178,
  CV_AMD64_XMM4_1 = 179,
  CV_AMD64_XMM4_2 = 180,
  CV_AMD64_XMM4_3 = 181,
  CV_AMD64_XMM5_0 = 182,
  CV_AMD64_XMM5_1 = 183,
  CV_AMD64_XMM5_2 = 184,
  CV_AMD64_XMM5_3 = 185,
  CV_AMD64_XMM6_0 = 186,
  CV_AMD64_XMM6_1 = 187,
  CV_AMD64_XMM6_2 = 188,
  CV_AMD64_XMM6_3 = 189,
  CV_AMD64_XMM7_0 = 190,
  CV_AMD64_XMM7_1 = 191,
  CV_AMD64_XMM7_2 = 192,
  CV_AMD64_XMM7_3 = 193,
  CV_AMD64_XMM0L = 194,
  CV_AMD64_XMM1L = 195,
  CV_AMD64_XMM2L = 196,
  CV_AMD64_XMM3L = 197,
  CV_AMD64_XMM4L = 198,
  CV_AMD64_XMM5L = 199,
  CV_AMD64_XMM6L = 200,
  CV_AMD64_XMM7L = 201,
  CV_AMD64_XMM0H = 202,
  CV_AMD64_XMM1H = 203,
  CV_AMD64_XMM2H = 204,
  CV_AMD64_XMM3H = 205,
  CV_AMD64_XMM4H = 206,
  CV_AMD64_XMM5H = 207,
  CV_AMD64_XMM6H = 208,
  CV_AMD64_XMM7H = 209,
  CV_AMD64_MXCSR = 211,
  CV_AMD64_EMM0L = 220,
  CV_AMD64_EMM1L = 221,
  CV_AMD64_EMM2L = 222,
  CV_AMD64_EMM3L = 223,
  CV_AMD64_EMM4L = 224,
  CV_AMD64_EMM5L = 225,
  CV_AMD64_EMM6L = 226,
  CV_AMD64_EMM7L = 227,
  CV_AMD64_EMM0H = 228,
  CV_AMD64_EMM1H = 229,
  CV_AMD64_EMM2H = 230,
  CV_AMD64_EMM3H = 231,
  CV_AMD64_EMM4H = 232,
  CV_AMD64_EMM5H = 233,
  CV_AMD64_EMM6H = 234,
  CV_AMD64_EMM7H = 235,
  CV_AMD64_MM00 = 236,
  CV_AMD64_MM01 = 237,
  CV_AMD64_MM10 = 238,
  CV_AMD64_MM11 = 239,
  CV_AMD64_MM20 = 240,
  CV_AMD64_MM21 = 241,
  CV_AMD64_MM30 = 242,
  CV_AMD64_MM31 = 243,
  CV_AMD64_MM40 = 244,
  CV_AMD64_MM41 = 245,
  CV_AMD64_MM50 = 246,
  CV_AMD64_MM51 = 247,
  CV_AMD64_MM60 = 248,
  CV_AMD64_MM61 = 249,
  CV_AMD64_MM70 = 250,
  CV_AMD64_MM71 = 251,
  CV_AMD64_XMM8 = 252,
  CV_AMD64_XMM9 = 253,
  CV_AMD64_XMM10 = 254,
  CV_AMD64_XMM11 = 255,
  CV_AMD64_XMM12 = 256,
  CV_AMD64_XMM13 = 257,
  CV_AMD64_XMM14 = 258,
  CV_AMD64_XMM15 = 259,
  CV_AMD64_XMM8_0 = 260,
  CV_AMD64_XMM8_1 = 261,
  CV_AMD64_XMM8_2 = 262,
  CV_AMD64_XMM8_3 = 263,
  CV_AMD64_XMM9_0 = 264,
  CV_AMD64_XMM9_1 = 265,
  CV_AMD64_XMM9_2 = 266,
  CV_AMD64_XMM9_3 = 267,
  CV_AMD64_XMM10_0 = 268,
  CV_AMD64_XMM10_1 = 269,
  CV_AMD64_XMM10_2 = 270,
  CV_AMD64_XMM10_3 = 271,
  CV_AMD64_XMM11_0 = 272,
  CV_AMD64_XMM11_1 = 273,
  CV_AMD64_XMM11_2 = 274,
  CV_AMD64_XMM11_3 = 275,
  CV_AMD64_XMM12_0 = 276,
  CV_AMD64_XMM12_1 = 277,
  CV_AMD64_XMM12_2 = 278,
  CV_AMD64_XMM12_3 = 279,
  CV_AMD64_XMM13_0 = 280,
  CV_AMD64_XMM13_1 = 281,
  CV_AMD64_XMM13_2 = 282,
  CV_AMD64_XMM13_3 = 283,
  CV_AMD64_XMM14_0 = 284,
  CV_AMD64_XMM14_1 = 285,
  CV_AMD64_XMM14_2 = 286,
  CV_AMD64_XMM14_3 = 287,
  CV_AMD64_XMM15_0 = 288,
  CV_AMD64_XMM15_1 = 289,
  CV_AMD64_XMM15_2 = 290,
  CV_AMD64_XMM15_3 = 291,
  CV_AMD64_XMM8L = 292,
  CV_AMD64_XMM9L = 293,
  CV_AMD64_XMM10L = 294,
  CV_AMD64_XMM11L = 295,
  CV_AMD64_XMM12L = 296,
  CV_AMD64_XMM13L = 297,
  CV_AMD64_XMM14L = 298,
  CV_AMD64_XMM15L = 299,
  CV_AMD64_XMM8H = 300,
  CV_AMD64_XMM9H = 301,
  CV_AMD64_XMM10H = 302,
  CV_AMD64_XMM11H = 303,
  CV_AMD64_XMM12H = 304,
  CV_AMD64_XMM13H = 305,
  CV_AMD64_XMM14H = 306,
  CV_AMD64_XMM15H = 307,
  CV_AMD64_EMM8L = 308,
  CV_AMD64_EMM9L = 309,
  CV_AMD64_EMM10L = 310,
  CV_AMD64_EMM11L = 311,
  CV_AMD64_EMM12L = 312,
  CV_AMD64_EMM13L = 313,
  CV_AMD64_EMM14L = 314,
  CV_AMD64_EMM15L = 315,
  CV_AMD64_EMM8H = 316,
  CV_AMD64_EMM9H = 317,
  CV_AMD64_EMM10H = 318,
  CV_AMD64_EMM11H = 319,
  CV_AMD64_EMM12H = 320,
  CV_AMD64_EMM13H = 321,
  CV_AMD64_EMM14H = 322,
  CV_AMD64_EMM15H = 323,
  CV_AMD64_SIL = 324,
  CV_AMD64_DIL = 325,
  CV_AMD64_BPL = 326,
  CV_AMD64_SPL = 327,
  CV_AMD64_RAX = 328,
  CV_AMD64_RBX = 329,
  CV_AMD64_RCX = 330,
  CV_AMD64_RDX = 331,
  CV_AMD64_RSI = 332,
  CV_AMD64_RDI = 333,
  CV_AMD64_RBP = 334,
  CV_AMD64_RSP = 335,
  CV_AMD64_R8 = 336,
  CV_AMD64_R9 = 337,
  CV_AMD64_R10 = 338,
  CV_AMD64_R11 = 339,
  CV_AMD64_R12 = 340,
  CV_AMD64_R13 = 341,
  CV_AMD64_R14 = 342,
  CV_AMD64_R15 = 343,
  CV_AMD64_R8B = 344,
  CV_AMD64_R9B = 345,
  CV_AMD64_R10B = 346,
  CV_AMD64_R11B = 347,
  CV_AMD64_R12B = 348,
  CV_AMD64_R13B = 349,
  CV_AMD64_R14B = 350,
  CV_AMD64_R15B = 351,
  CV_AMD64_R8W = 352,
  CV_AMD64_R9W = 353,
  CV_AMD64_R10W = 354,
  CV_AMD64_R11W = 355,
  CV_AMD64_R12W = 356,
  CV_AMD64_R13W = 357,
  CV_AMD64_R14W = 358,
  CV_AMD64_R15W = 359,
  CV_AMD64_R8D = 360,
  CV_AMD64_R9D = 361,
  CV_AMD64_R10D = 362,
  CV_AMD64_R11D = 363,
  CV_AMD64_R12D = 364,
  CV_AMD64_R13D = 365,
  CV_AMD64_R14D = 366,
  CV_AMD64_R15D = 367,
  CV_AMD64_YMM0 = 368,
  CV_AMD64_YMM1 = 369,
  CV_AMD64_YMM2 = 370,
  CV_AMD64_YMM3 = 371,
  CV_AMD64_YMM4 = 372,
  CV_AMD64_YMM5 = 373,
  CV_AMD64_YMM6 = 374,
  CV_AMD64_YMM7 = 375,
  CV_AMD64_YMM8 = 376,
  CV_AMD64_YMM9 = 377,
  CV_AMD64_YMM10 = 378,
  CV_AMD64_YMM11 = 379,
  CV_AMD64_YMM12 = 380,
  CV_AMD64_YMM13 = 381,
  CV_AMD64_YMM14 = 382,
  CV_AMD64_YMM15 = 383,
  CV_AMD64_YMM0H = 384,
  CV_AMD64_YMM1H = 385,
  CV_AMD64_YMM2H = 386,
  CV_AMD64_YMM3H = 387,
  CV_AMD64_YMM4H = 388,
  CV_AMD64_YMM5H = 389,
  CV_AMD64_YMM6H = 390,
  CV_AMD64_YMM7H = 391,
  CV_AMD64_YMM8H = 392,
  CV_AMD64_YMM9H = 393,
  CV_AMD64_YMM10H = 394,
  CV_AMD64_YMM11H = 395,
  CV_AMD64_YMM12H = 396,
  CV_AMD64_YMM13H = 397,
  CV_AMD64_YMM14H = 398,
  CV_AMD64_YMM15H = 399,
  CV_AMD64_XMM0IL = 400,
  CV_AMD64_XMM1IL = 401,
  CV_AMD64_XMM2IL = 402,
  CV_AMD64_XMM3IL = 403,
  CV_AMD64_XMM4IL = 404,
  CV_AMD64_XMM5IL = 405,
  CV_AMD64_XMM6IL = 406,
  CV_AMD64_XMM7IL = 407,
  CV_AMD64_XMM8IL = 408,
  CV_AMD64_XMM9IL = 409,
  CV_AMD64_XMM10IL = 410,
  CV_AMD64_XMM11IL = 411,
  CV_AMD64_XMM12IL = 412,
  CV_AMD64_XMM13IL = 413,
  CV_AMD64_XMM14IL = 414,
  CV_AMD64_XMM15IL = 415,
  CV_AMD64_XMM0IH = 416,
  CV_AMD64_XMM1IH = 417,
  CV_AMD64_XMM2IH = 418,
  CV_AMD64_XMM3IH = 419,
  CV_AMD64_XMM4IH = 420,
  CV_AMD64_XMM5IH = 421,
  CV_AMD64_XMM6IH = 422,
  CV_AMD64_XMM7IH = 423,
  CV_AMD64_XMM8IH = 424,
  CV_AMD64_XMM9IH = 425,
  CV_AMD64_XMM10IH = 426,
  CV_AMD64_XMM11IH = 427,
  CV_AMD64_XMM12IH = 428,
  CV_AMD64_XMM13IH = 429,
  CV_AMD64_XMM14IH = 430,
  CV_AMD64_XMM15IH = 431,
  CV_AMD64_YMM0I0 = 432,
  CV_AMD64_YMM0I1 = 433,
  CV_AMD64_YMM0I2 = 434,
  CV_AMD64_YMM0I3 = 435,
  CV_AMD64_YMM1I0 = 436,
  CV_AMD64_YMM1I1 = 437,
  CV_AMD64_YMM1I2 = 438,
  CV_AMD64_YMM1I3 = 439,
  CV_AMD64_YMM2I0 = 440,
  CV_AMD64_YMM2I1 = 441,
  CV_AMD64_YMM2I2 = 442,
  CV_AMD64_YMM2I3 = 443,
  CV_AMD64_YMM3I0 = 444,
  CV_AMD64_YMM3I1 = 445,
  CV_AMD64_YMM3I2 = 446,
  CV_AMD64_YMM3I3 = 447,
  CV_AMD64_YMM4I0 = 448,
  CV_AMD64_YMM4I1 = 449,
  CV_AMD64_YMM4I2 = 450,
  CV_AMD64_YMM4I3 = 451,
  CV_AMD64_YMM5I0 = 452,
  CV_AMD64_YMM5I1 = 453,
  CV_AMD64_YMM5I2 = 454,
  CV_AMD64_YMM5I3 = 455,
  CV_AMD64_YMM6I0 = 456,
  CV_AMD64_YMM6I1 = 457,
  CV_AMD64_YMM6I2 = 458,
  CV_AMD64_YMM6I3 = 459,
  CV_AMD64_YMM7I0 = 460,
  CV_AMD64_YMM7I1 = 461,
  CV_AMD64_YMM7I2 = 462,
  CV_AMD64_YMM7I3 = 463,
  CV_AMD64_YMM8I0 = 464,
  CV_AMD64_YMM8I1 = 465,
  CV_AMD64_YMM8I2 = 466,
  CV_AMD64_YMM8I3 = 467,
  CV_AMD64_YMM9I0 = 468,
  CV_AMD64_YMM9I1 = 469,
  CV_AMD64_YMM9I2 = 470,
  CV_AMD64_YMM9I3 = 471,
  CV_AMD64_YMM10I0 = 472,
  CV_AMD64_YMM10I1 = 473,
  CV_AMD64_YMM10I2 = 474,
  CV_AMD64_YMM10I3 = 475,
  CV_AMD64_YMM11I0 = 476,
  CV_AMD64_YMM11I1 = 477,
  CV_AMD64_YMM11I2 = 478,
  CV_AMD64_YMM11I3 = 479,
  CV_AMD64_YMM12I0 = 480,
  CV_AMD64_YMM12I1 = 481,
  CV_AMD64_YMM12I2 = 482,
  CV_AMD64_YMM12I3 = 483,
  CV_AMD64_YMM13I0 = 484,
  CV_AMD64_YMM13I1 = 485,
  CV_AMD64_YMM13I2 = 486,
  CV_AMD64_YMM13I3 = 487,
  CV_AMD64_YMM14I0 = 488,
  CV_AMD64_YMM14I1 = 489,
  CV_AMD64_YMM14I2 = 490,
  CV_AMD64_YMM14I3 = 491,
  CV_AMD64_YMM15I0 = 492,
  CV_AMD64_YMM15I1 = 493,
  CV_AMD64_YMM15I2 = 494,
  CV_AMD64_YMM15I3 = 495,
  CV_AMD64_YMM0F0 = 496,
  CV_AMD64_YMM0F1 = 497,
  CV_AMD64_YMM0F2 = 498,
  CV_AMD64_YMM0F3 = 499,
  CV_AMD64_YMM0F4 = 500,
  CV_AMD64_YMM0F5 = 501,
  CV_AMD64_YMM0F6 = 502,
  CV_AMD64_YMM0F7 = 503,
  CV_AMD64_YMM1F0 = 504,
  CV_AMD64_YMM1F1 = 505,
  CV_AMD64_YMM1F2 = 506,
  CV_AMD64_YMM1F3 = 507,
  CV_AMD64_YMM1F4 = 508,
  CV_AMD64_YMM1F5 = 509,
  CV_AMD64_YMM1F6 = 510,
  CV_AMD64_YMM1F7 = 511,
  CV_AMD64_YMM2F0 = 512,
  CV_AMD64_YMM2F1 = 513,
  CV_AMD64_YMM2F2 = 514,
  CV_AMD64_YMM2F3 = 515,
  CV_AMD64_YMM2F4 = 516,
  CV_AMD64_YMM2F5 = 517,
  CV_AMD64_YMM2F6 = 518,
  CV_AMD64_YMM2F7 = 519,
  CV_AMD64_YMM3F0 = 520,
  CV_AMD64_YMM3F1 = 521,
  CV_AMD64_YMM3F2 = 522,
  CV_AMD64_YMM3F3 = 523,
  CV_AMD64_YMM3F4 = 524,
  CV_AMD64_YMM3F5 = 525,
  CV_AMD64_YMM3F6 = 526,
  CV_AMD64_YMM3F7 = 527,
  CV_AMD64_YMM4F0 = 528,
  CV_AMD64_YMM4F1 = 529,
  CV_AMD64_YMM4F2 = 530,
  CV_AMD64_YMM4F3 = 531,
  CV_AMD64_YMM4F4 = 532,
  CV_AMD64_YMM4F5 = 533,
  CV_AMD64_YMM4F6 = 534,
  CV_AMD64_YMM4F7 = 535,
  CV_AMD64_YMM5F0 = 536,
  CV_AMD64_YMM5F1 = 537,
  CV_AMD64_YMM5F2 = 538,
  CV_AMD64_YMM5F3 = 539,
  CV_AMD64_YMM5F4 = 540,
  CV_AMD64_YMM5F5 = 541,
  CV_AMD64_YMM5F6 = 542,
  CV_AMD64_YMM5F7 = 543,
  CV_AMD64_YMM6F0 = 544,
  CV_AMD64_YMM6F1 = 545,
  CV_AMD64_YMM6F2 = 546,
  CV_AMD64_YMM6F3 = 547,
  CV_AMD64_YMM6F4 = 548,
  CV_AMD64_YMM6F5 = 549,
  CV_AMD64_YMM6F6 = 550,
  CV_AMD64_YMM6F7 = 551,
  CV_AMD64_YMM7F0 = 552,
  CV_AMD64_YMM7F1 = 553,
  CV_AMD64_YMM7F2 = 554,
  CV_AMD64_YMM7F3 = 555,
  CV_AMD64_YMM7F4 = 556,
  CV_AMD64_YMM7F5 = 557,
  CV_AMD64_YMM7F6 = 558,
  CV_AMD64_YMM7F7 = 559,
  CV_AMD64_YMM8F0 = 560,
  CV_AMD64_YMM8F1 = 561,
  CV_AMD64_YMM8F2 = 562,
  CV_AMD64_YMM8F3 = 563,
  CV_AMD64_YMM8F4 = 564,
  CV_AMD64_YMM8F5 = 565,
  CV_AMD64_YMM8F6 = 566,
  CV_AMD64_YMM8F7 = 567,
  CV_AMD64_YMM9F0 = 568,
  CV_AMD64_YMM9F1 = 569,
  CV_AMD64_YMM9F2 = 570,
  CV_AMD64_YMM9F3 = 571,
  CV_AMD64_YMM9F4 = 572,
  CV_AMD64_YMM9F5 = 573,
  CV_AMD64_YMM9F6 = 574,
  CV_AMD64_YMM9F7 = 575,
  CV_AMD64_YMM10F0 = 576,
  CV_AMD64_YMM10F1 = 577,
  CV_AMD64_YMM10F2 = 578,
  CV_AMD64_YMM10F3 = 579,
  CV_AMD64_YMM10F4 = 580,
  CV_AMD64_YMM10F5 = 581,
  CV_AMD64_YMM10F6 = 582,
  CV_AMD64_YMM10F7 = 583,
  CV_AMD64_YMM11F0 = 584,
  CV_AMD64_YMM11F1 = 585,
  CV_AMD64_YMM11F2 = 586,
  CV_AMD64_YMM11F3 = 587,
  CV_AMD64_YMM11F4 = 588,
  CV_AMD64_YMM11F5 = 589,
  CV_AMD64_YMM11F6 = 590,
  CV_AMD64_YMM11F7 = 591,
  CV_AMD64_YMM12F0 = 592,
  CV_AMD64_YMM12F1 = 593,
  CV_AMD64_YMM12F2 = 594,
  CV_AMD64_YMM12F3 = 595,
  CV_AMD64_YMM12F4 = 596,
  CV_AMD64_YMM12F5 = 597,
  CV_AMD64_YMM12F6 = 598,
  CV_AMD64_YMM12F7 = 599,
  CV_AMD64_YMM13F0 = 600,
  CV_AMD64_YMM13F1 = 601,
  CV_AMD64_YMM13F2 = 602,
  CV_AMD64_YMM13F3 = 603,
  CV_AMD64_YMM13F4 = 604,
  CV_AMD64_YMM13F5 = 605,
  CV_AMD64_YMM13F6 = 606,
  CV_AMD64_YMM13F7 = 607,
  CV_AMD64_YMM14F0 = 608,
  CV_AMD64_YMM14F1 = 609,
  CV_AMD64_YMM14F2 = 610,
  CV_AMD64_YMM14F3 = 611,
  CV_AMD64_YMM14F4 = 612,
  CV_AMD64_YMM14F5 = 613,
  CV_AMD64_YMM14F6 = 614,
  CV_AMD64_YMM14F7 = 615,
  CV_AMD64_YMM15F0 = 616,
  CV_AMD64_YMM15F1 = 617,
  CV_AMD64_YMM15F2 = 618,
  CV_AMD64_YMM15F3 = 619,
  CV_AMD64_YMM15F4 = 620,
  CV_AMD64_YMM15F5 = 621,
  CV_AMD64_YMM15F6 = 622,
  CV_AMD64_YMM15F7 = 623,
  CV_AMD64_YMM0D0 = 624,
  CV_AMD64_YMM0D1 = 625,
  CV_AMD64_YMM0D2 = 626,
  CV_AMD64_YMM0D3 = 627,
  CV_AMD64_YMM1D0 = 628,
  CV_AMD64_YMM1D1 = 629,
  CV_AMD64_YMM1D2 = 630,
  CV_AMD64_YMM1D3 = 631,
  CV_AMD64_YMM2D0 = 632,
  CV_AMD64_YMM2D1 = 633,
  CV_AMD64_YMM2D2 = 634,
  CV_AMD64_YMM2D3 = 635,
  CV_AMD64_YMM3D0 = 636,
  CV_AMD64_YMM3D1 = 637,
  CV_AMD64_YMM3D2 = 638,
  CV_AMD64_YMM3D3 = 639,
  CV_AMD64_YMM4D0 = 640,
  CV_AMD64_YMM4D1 = 641,
  CV_AMD64_YMM4D2 = 642,
  CV_AMD64_YMM4D3 = 643,
  CV_AMD64_YMM5D0 = 644,
  CV_AMD64_YMM5D1 = 645,
  CV_AMD64_YMM5D2 = 646,
  CV_AMD64_YMM5D3 = 647,
  CV_AMD64_YMM6D0 = 648,
  CV_AMD64_YMM6D1 = 649,
  CV_AMD64_YMM6D2 = 650,
  CV_AMD64_YMM6D3 = 651,
  CV_AMD64_YMM7D0 = 652,
  CV_AMD64_YMM7D1 = 653,
  CV_AMD64_YMM7D2 = 654,
  CV_AMD64_YMM7D3 = 655,
  CV_AMD64_YMM8D0 = 656,
  CV_AMD64_YMM8D1 = 657,
  CV_AMD64_YMM8D2 = 658,
  CV_AMD64_YMM8D3 = 659,
  CV_AMD64_YMM9D0 = 660,
  CV_AMD64_YMM9D1 = 661,
  CV_AMD64_YMM9D2 = 662,
  CV_AMD64_YMM9D3 = 663,
  CV_AMD64_YMM10D0 = 664,
  CV_AMD64_YMM10D1 = 665,
  CV_AMD64_YMM10D2 = 666,
  CV_AMD64_YMM10D3 = 667,
  CV_AMD64_YMM11D0 = 668,
  CV_AMD64_YMM11D1 = 669,
  CV_AMD64_YMM11D2 = 670,
  CV_AMD64_YMM11D3 = 671,
  CV_AMD64_YMM12D0 = 672,
  CV_AMD64_YMM12D1 = 673,
  CV_AMD64_YMM12D2 = 674,
  CV_AMD64_YMM12D3 = 675,
  CV_AMD64_YMM13D0 = 676,
  CV_AMD64_YMM13D1 = 677,
  CV_AMD64_YMM13D2 = 678,
  CV_AMD64_YMM13D3 = 679,
  CV_AMD64_YMM14D0 = 680,
  CV_AMD64_YMM14D1 = 681,
  CV_AMD64_YMM14D2 = 682,
  CV_AMD64_YMM14D3 = 683,
  CV_AMD64_YMM15D0 = 684,
  CV_AMD64_YMM15D1 = 685,
  CV_AMD64_YMM15D2 = 686,
  CV_AMD64_YMM15D3 = 687
};

/* This is enum BinaryAnnotationOpcode in Microsoft's cvinfo.h.  */

enum binary_annotation_opcode {
  ba_op_invalid,
  ba_op_code_offset,
  ba_op_change_code_offset_base,
  ba_op_change_code_offset,
  ba_op_change_code_length,
  ba_op_change_file,
  ba_op_change_line_offset,
  ba_op_change_line_end_delta,
  ba_op_change_range_kind,
  ba_op_change_column_start,
  ba_op_change_column_end_delta,
  ba_op_change_code_offset_and_line_offset,
  ba_op_change_code_length_and_code_offset,
  ba_op_change_column_end
};

struct codeview_string
{
  codeview_string *next;
  uint32_t offset;
  char *string;
};

struct string_hasher : free_ptr_hash <struct codeview_string>
{
  typedef const char *compare_type;

  static hashval_t hash (const codeview_string *x)
  {
    return htab_hash_string (x->string);
  }

  static bool equal (const codeview_string *x, const char *y)
  {
    return !strcmp (x->string, y);
  }

  static void mark_empty (codeview_string *x)
  {
    if (x->string)
      {
	free (x->string);
	x->string = NULL;
      }
  }

  static void remove (codeview_string *&x)
  {
    free (x->string);
  }
};

struct codeview_source_file
{
  codeview_source_file *next;
  unsigned int file_num;
  uint32_t string_offset;
  char *filename;
  uint8_t hash[HASH_SIZE];
};

struct codeview_line
{
  codeview_line *next;
  unsigned int line_no;
  unsigned int label_num;
};

struct codeview_line_block
{
  codeview_line_block *next;
  uint32_t file_id;
  unsigned int num_lines;
  codeview_line *lines, *last_line;
};

struct codeview_inlinee_lines
{
  codeview_inlinee_lines *next;
  uint32_t func_id;
  uint32_t file_id;
  uint32_t starting_line;
};

struct codeview_function
{
  codeview_function *next;
  codeview_function *htab_next;
  function *func;
  unsigned int end_label;
  codeview_line_block *blocks, *last_block;
  codeview_function *parent;
  unsigned int inline_block;
  location_t inline_loc;
};

struct codeview_symbol
{
  codeview_symbol *next;
  enum cv_sym_type kind;

  union
  {
    struct
    {
      uint32_t type;
      char *name;
      dw_die_ref die;
    } data_symbol;
    struct
    {
      uint32_t parent;
      uint32_t end;
      uint32_t next;
      uint32_t type;
      uint8_t flags;
      char *name;
      dw_die_ref die;
    } function;
  };
};

struct codeview_type
{
  dw_die_ref die;
  uint32_t num;
  bool is_fwd_ref;
};

struct die_hasher : free_ptr_hash <codeview_type>
{
  typedef dw_die_ref compare_type;

  static hashval_t hash (const codeview_type *x)
  {
    return htab_hash_pointer (x->die);
  }

  static bool equal (const codeview_type *x, const dw_die_ref y)
  {
    return x->die == y;
  }
};

struct codeview_integer
{
  bool neg;
  uint64_t num;
};

struct codeview_subtype
{
  struct codeview_subtype *next;
  enum cv_leaf_type kind;

  union
  {
    struct
    {
      char *name;
      struct codeview_integer value;
    } lf_enumerate;
    struct
    {
      uint32_t type_num;
    } lf_index;
    struct
    {
      uint16_t attributes;
      uint32_t type;
      codeview_integer offset;
      char *name;
    } lf_member;
    struct
    {
      uint16_t attributes;
      uint32_t type;
      char *name;
    } lf_static_member;
    struct
    {
      uint16_t method_attribute;
      uint32_t method_type;
      char *name;
    } lf_onemethod;
    struct
    {
      uint16_t count;
      uint32_t method_list;
      char *name;
    } lf_method;
    struct
    {
      uint16_t attributes;
      uint32_t base_class_type;
      codeview_integer offset;
    } lf_bclass;
    struct
    {
      uint32_t type;
      char *name;
    } lf_nesttype;
  };
};

struct lf_methodlist_entry
{
  uint16_t method_attribute;
  uint32_t method_type;
};

struct codeview_custom_type
{
  struct codeview_custom_type *next;
  uint32_t num;
  enum cv_leaf_type kind;

  union
  {
    struct
    {
      uint32_t base_type;
      uint32_t attributes;
      uint32_t containing_class;
      uint16_t ptr_to_mem_type;
    } lf_pointer;
    struct
    {
      uint32_t base_type;
      uint16_t modifier;
    } lf_modifier;
    struct
    {
      size_t length;
      codeview_subtype *subtypes;
      codeview_subtype *last_subtype;
    } lf_fieldlist;
    struct
    {
      uint16_t count;
      uint16_t properties;
      uint32_t underlying_type;
      uint32_t fieldlist;
      char *name;
    } lf_enum;
    struct
    {
      uint16_t num_members;
      uint16_t properties;
      uint32_t field_list;
      uint32_t derived_from;
      uint32_t vshape;
      codeview_integer length;
      char *name;
    } lf_structure;
    struct
    {
      uint32_t element_type;
      uint32_t index_type;
      codeview_integer length_in_bytes;
    } lf_array;
    struct
    {
      uint32_t base_type;
      uint8_t length;
      uint8_t position;
    } lf_bitfield;
    struct
    {
      uint32_t return_type;
      uint8_t calling_convention;
      uint8_t attributes;
      uint16_t num_parameters;
      uint32_t arglist;
    } lf_procedure;
    struct
    {
      uint32_t num_entries;
      uint32_t *args;
    } lf_arglist;
    struct
    {
      uint32_t parent_scope;
      uint32_t function_type;
      char *name;
    } lf_func_id;
    struct
    {
      uint32_t parent_type;
      uint32_t function_type;
      char *name;
    } lf_mfunc_id;
    struct
    {
      uint32_t substring;
      char *string;
    } lf_string_id;
    struct
    {
      uint32_t return_type;
      uint32_t containing_class_type;
      uint32_t this_type;
      uint8_t calling_convention;
      uint8_t attributes;
      uint16_t num_parameters;
      uint32_t arglist;
      int32_t this_adjustment;
    } lf_mfunction;
    struct
    {
      unsigned int count;
      lf_methodlist_entry *entries;
    } lf_methodlist;
  };
};

struct codeview_deferred_type
{
  struct codeview_deferred_type *next;
  dw_die_ref type;
};

struct string_id_hasher : nofree_ptr_hash <struct codeview_custom_type>
{
  typedef const char *compare_type;

  static hashval_t hash (const codeview_custom_type *x)
  {
    return htab_hash_string (x->lf_string_id.string);
  }

  static bool equal (const codeview_custom_type *x, const char *y)
  {
    return !strcmp (x->lf_string_id.string, y);
  }
};

struct codeview_method
{
  uint16_t attribute;
  uint32_t type;
  char *name;
  unsigned int count;
  struct codeview_method *next;
  struct codeview_method *last;
};

struct method_hasher : nofree_ptr_hash <struct codeview_method>
{
  typedef const char *compare_type;

  static hashval_t hash (const codeview_method *x)
  {
    return htab_hash_string (x->name);
  }

  static bool equal (const codeview_method *x, const char *y)
  {
    return !strcmp (x->name, y);
  }
};

struct inlinee_lines_hasher : free_ptr_hash <struct codeview_inlinee_lines>
{
  typedef uint32_t compare_type;

  static hashval_t hash (const codeview_inlinee_lines *il)
  {
    return il->func_id;
  }

  static bool equal (const codeview_inlinee_lines *il, uint32_t func_id)
  {
    return il->func_id == func_id;
  }
};

struct cv_func_hasher : nofree_ptr_hash <struct codeview_function>
{
  typedef dw_die_ref compare_type;

  static bool equal (const codeview_function *f, dw_die_ref die)
  {
    return lookup_decl_die (f->func->decl) == die;
  }
};

static unsigned int line_label_num;
static unsigned int func_label_num;
static unsigned int sym_label_num;
static codeview_source_file *files, *last_file;
static unsigned int num_files;
static uint32_t string_offset = 1;
static hash_table<string_hasher> *strings_htab;
static codeview_string *strings, *last_string;
static codeview_function *funcs, *last_func, *cur_func;
static const char* last_filename;
static uint32_t last_file_id;
static codeview_symbol *sym, *last_sym;
static hash_table<die_hasher> *types_htab, *func_htab;
static codeview_custom_type *custom_types, *last_custom_type;
static codeview_deferred_type *deferred_types, *last_deferred_type;
static hash_table<string_id_hasher> *string_id_htab;
static hash_table<inlinee_lines_hasher> *inlinee_lines_htab;
static hash_table<cv_func_hasher> *cv_func_htab;

static uint32_t get_type_num (dw_die_ref type, bool in_struct, bool no_fwd_ref);
static uint32_t get_type_num_subroutine_type (dw_die_ref type, bool in_struct,
					      uint32_t containing_class_type,
					      uint32_t this_type,
					      int32_t this_adjustment);
static void write_cv_padding (size_t padding);
static void flush_deferred_types (void);
static uint32_t get_func_id (dw_die_ref die);
static void write_inlinesite_records (dw_die_ref func, dw_die_ref die);

/* Return the file ID corresponding to a given source filename.  */

static uint32_t
get_file_id (const char *filename)
{
  codeview_source_file *sf = files;

  if (filename == last_filename)
    return last_file_id;

  while (sf)
    {
      if (!strcmp (sf->filename, filename))
	{
	  uint32_t file_id;

	  /* 0x18 is the size of the checksum entry for each file.
	     0x6 bytes for the header, plus 0x10 bytes for the hash,
	     then padded to a multiple of 4.  */

	  file_id = sf->file_num * 0x18;
	  last_filename = filename;
	  last_file_id = file_id;

	  return file_id;
	}

      sf = sf->next;
    }

  return 0;
}

/* Allocate and initialize a codeview_function struct.  */

static codeview_function *
new_codeview_function (void)
{
  codeview_function **slot;
  dw_die_ref die;
  codeview_function *f = (codeview_function *)
			    xmalloc (sizeof (codeview_function));

  f->next = NULL;
  f->htab_next = NULL;
  f->func = cfun;
  f->end_label = 0;
  f->blocks = f->last_block = NULL;
  f->inline_block = 0;
  f->inline_loc = 0;

  if (!funcs)
    funcs = f;
  else
    last_func->next = f;

  last_func = f;

  if (!cv_func_htab)
    cv_func_htab = new hash_table<cv_func_hasher> (10);

  die = lookup_decl_die (cfun->decl);

  slot = cv_func_htab->find_slot_with_hash (die, htab_hash_pointer (die),
					    INSERT);
  if (*slot)
    f->htab_next = *slot;

  *slot = f;

  return f;
}

/* Record new line number against the current function.  */

void
codeview_source_line (unsigned int line_no, const char *filename)
{
  codeview_line *l;
  uint32_t file_id = get_file_id (filename);
  unsigned int label_num = ++line_label_num;

  targetm.asm_out.internal_label (asm_out_file, LINE_LABEL, label_num);

  if (!cur_func || cur_func->func != cfun)
    {
      codeview_function *f = new_codeview_function ();

      f->parent = NULL;

      cur_func = f;
    }

  if (!cur_func->last_block || cur_func->last_block->file_id != file_id)
    {
      codeview_line_block *b;

      b = (codeview_line_block *) xmalloc (sizeof (codeview_line_block));

      b->next = NULL;
      b->file_id = file_id;
      b->num_lines = 0;
      b->lines = b->last_line = NULL;

      if (!cur_func->blocks)
	cur_func->blocks = b;
      else
	cur_func->last_block->next = b;

      cur_func->last_block = b;
    }

  if (cur_func->last_block->last_line
    && cur_func->last_block->last_line->line_no == line_no)
    return;

  l = (codeview_line *) xmalloc (sizeof (codeview_line));

  l->next = NULL;
  l->line_no = line_no;
  l->label_num = label_num;

  if (!cur_func->last_block->lines)
    cur_func->last_block->lines = l;
  else
    cur_func->last_block->last_line->next = l;

  cur_func->last_block->last_line = l;
  cur_func->last_block->num_lines++;
}

/* We have encountered the beginning of a lexical block.  If this is actually
   an inlined function, allocate a new codeview_function for this.  */

void
codeview_begin_block (unsigned int line ATTRIBUTE_UNUSED,
		      unsigned int blocknum, tree block)
{
  if (inlined_function_outer_scope_p (block))
    {
      location_t locus = BLOCK_SOURCE_LOCATION (block);
      expanded_location s = expand_location (locus);
      codeview_function *f = new_codeview_function ();

      codeview_source_line (s.line, s.file);

      f->parent = cur_func;
      f->inline_block = blocknum;
      f->inline_loc = locus;

      cur_func = f;
    }
}

/* We have encountered the end of a lexical block.  If this is the end of an
   inlined function, change cur_func back to its parent.  */

void
codeview_end_block (unsigned int line ATTRIBUTE_UNUSED, unsigned int blocknum)
{
  if (cur_func && cur_func->inline_block == blocknum)
    {
      /* If inlined function, add dummy source line at the end so we know how
	 long the actual last line is.  */
      codeview_source_line (0, "");

      cur_func = cur_func->parent;
    }
}

/* Adds string to the string table, returning its offset.  If already present,
   this returns the offset of the existing string.  */

static uint32_t
add_string (const char *string)
{
  codeview_string **slot;
  codeview_string *s;
  size_t len;

  if (!strings_htab)
    strings_htab = new hash_table<string_hasher> (10);

  slot = strings_htab->find_slot_with_hash (string, htab_hash_string (string),
					    INSERT);

  if (*slot)
    return (*slot)->offset;

  s = (codeview_string *) xmalloc (sizeof (codeview_string));
  len = strlen (string);

  s->next = NULL;

  s->offset = string_offset;
  string_offset += len + 1;

  s->string = xstrdup (string);

  if (last_string)
    last_string->next = s;
  else
    strings = s;

  last_string = s;

  *slot = s;

  return s->offset;
}

/* A new source file has been encountered - record the details and calculate
   its hash.  */

void
codeview_start_source_file (const char *filename)
{
  codeview_source_file *sf;
  char *path;
  uint32_t string_offset;
  FILE *f;

  path = lrealpath (filename);
  string_offset = add_string (path);
  free (path);

  sf = files;
  while (sf)
    {
      if (sf->string_offset == string_offset)
	return;

      sf = sf->next;
    }

  sf = (codeview_source_file *) xmalloc (sizeof (codeview_source_file));
  sf->next = NULL;
  sf->file_num = num_files;
  sf->string_offset = string_offset;
  sf->filename = xstrdup (filename);

  f = fopen (filename, "r");
  if (!f)
    internal_error ("could not open %s for reading", filename);

  if (md5_stream (f, sf->hash))
    {
      fclose (f);
      internal_error ("md5_stream failed");
    }

  fclose (f);

  if (last_file)
    last_file->next = sf;
  else
    files = sf;

  last_file = sf;
  num_files++;
}

/* Write out the strings table into the .debug$S section.  The linker will
   parse this, and handle the deduplication and hashing for all the object
   files.  */

static void
write_strings_table (void)
{
  codeview_string *string;

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, DEBUG_S_STRINGTABLE);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_strings_end - %LLcv_strings_start\n");

  asm_fprintf (asm_out_file, "%LLcv_strings_start:\n");

  /* The first entry is always an empty string.  */
  fputs (integer_asm_op (1, false), asm_out_file);
  fprint_whex (asm_out_file, 0);
  putc ('\n', asm_out_file);

  string = strings;
  while (string)
    {
      ASM_OUTPUT_ASCII (asm_out_file, string->string,
			strlen (string->string) + 1);

      string = string->next;
    }

  delete strings_htab;

  asm_fprintf (asm_out_file, "%LLcv_strings_end:\n");

  ASM_OUTPUT_ALIGN (asm_out_file, 2);
}

/* Write out the file checksums data into the .debug$S section.  */

static void
write_source_files (void)
{
  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, DEBUG_S_FILECHKSMS);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  asm_fprintf (asm_out_file,
	       "%LLcv_filechksms_end - %LLcv_filechksms_start\n");

  asm_fprintf (asm_out_file, "%LLcv_filechksms_start:\n");

  while (files)
    {
      codeview_source_file *next = files->next;

      /* This is struct file_checksum in binutils, or filedata in Microsoft's
	 dumpsym7.cpp:

	struct file_checksum
	{
	  uint32_t file_id;
	  uint8_t checksum_length;
	  uint8_t checksum_type;
	} ATTRIBUTE_PACKED;

	followed then by the bytes of the hash, padded to the next 4 bytes.
	file_id here is actually the offset in the strings table.  */

      fputs (integer_asm_op (4, false), asm_out_file);
      fprint_whex (asm_out_file, files->string_offset);
      putc ('\n', asm_out_file);

      fputs (integer_asm_op (1, false), asm_out_file);
      fprint_whex (asm_out_file, HASH_SIZE);
      putc ('\n', asm_out_file);

      fputs (integer_asm_op (1, false), asm_out_file);
      fprint_whex (asm_out_file, CHKSUM_TYPE_MD5);
      putc ('\n', asm_out_file);

      for (unsigned int i = 0; i < HASH_SIZE; i++)
	{
	  fputs (integer_asm_op (1, false), asm_out_file);
	  fprint_whex (asm_out_file, files->hash[i]);
	  putc ('\n', asm_out_file);
	}

      ASM_OUTPUT_ALIGN (asm_out_file, 2);

      free (files->filename);
      free (files);

      files = next;
    }

  asm_fprintf (asm_out_file, "%LLcv_filechksms_end:\n");
}

/* Write out the line number information for each function into the
   .debug$S section.  */

static void
write_line_numbers (void)
{
  unsigned int func_num = 0;
  codeview_function *f = funcs;

  while (f)
    {
      codeview_function *next_func = f->next;
      unsigned int first_label_num;
      codeview_line_block *b = f->blocks;

      if (f->inline_block != 0)
	{
	  f = next_func;
	  continue;
	}

      fputs (integer_asm_op (4, false), asm_out_file);
      fprint_whex (asm_out_file, DEBUG_S_LINES);
      putc ('\n', asm_out_file);

      fputs (integer_asm_op (4, false), asm_out_file);
      asm_fprintf (asm_out_file, "%LLcv_lines%u_end - %LLcv_lines%u_start\n",
		   func_num, func_num);

      asm_fprintf (asm_out_file, "%LLcv_lines%u_start:\n", func_num);

      /* Output the header (struct cv_lines_header in binutils or
	 CV_DebugSLinesHeader_t in Microsoft's cvinfo.h):

	struct cv_lines_header
	{
	  uint32_t offset;
	  uint16_t section;
	  uint16_t flags;
	  uint32_t length;
	};
      */

      asm_fprintf (asm_out_file, "\t.secrel32\t%L" LINE_LABEL "%u\n",
		   b->lines->label_num);
      asm_fprintf (asm_out_file, "\t.secidx\t%L" LINE_LABEL "%u\n",
		   b->lines->label_num);

      /* flags */
      fputs (integer_asm_op (2, false), asm_out_file);
      fprint_whex (asm_out_file, 0);
      putc ('\n', asm_out_file);

      first_label_num = b->lines->label_num;

      /* length */
      fputs (integer_asm_op (4, false), asm_out_file);
      asm_fprintf (asm_out_file,
		   "%L" END_FUNC_LABEL "%u - %L" LINE_LABEL "%u\n",
		   f->end_label, first_label_num);

      while (b)
	{
	  codeview_line_block *next_block = b->next;
	  codeview_line *l = b->lines;

	  /* Next comes the blocks, each block being a part of a function
	     within the same source file (struct cv_lines_block in binutils or
	     CV_DebugSLinesFileBlockHeader_t in Microsoft's cvinfo.h):

	    struct cv_lines_block
	    {
	      uint32_t file_id;
	      uint32_t num_lines;
	      uint32_t length;
	    };
	  */

	  /* file ID */
	  fputs (integer_asm_op (4, false), asm_out_file);
	  fprint_whex (asm_out_file, b->file_id);
	  putc ('\n', asm_out_file);

	  /* number of lines */
	  fputs (integer_asm_op (4, false), asm_out_file);
	  fprint_whex (asm_out_file, b->num_lines);
	  putc ('\n', asm_out_file);

	  /* length of code block: (num_lines * sizeof (struct cv_line)) +
	     sizeof (struct cv_lines_block) */
	  fputs (integer_asm_op (4, false), asm_out_file);
	  fprint_whex (asm_out_file, (b->num_lines * 0x8) + 0xc);
	  putc ('\n', asm_out_file);

	  while (l)
	    {
	      codeview_line *next_line = l->next;

	      /* Finally comes the line number information (struct cv_line in
		 binutils or CV_Line_t in Microsoft's cvinfo.h):

		struct cv_line
		{
		  uint32_t offset;
		  uint32_t line_no;
		};

		Strictly speaking line_no is a bitfield: the bottom 24 bits
		are the line number, and the top bit means "is a statement".
	      */

	      fputs (integer_asm_op (4, false), asm_out_file);
	      asm_fprintf (asm_out_file,
			   "%L" LINE_LABEL "%u - %L" LINE_LABEL "%u\n",
			   l->label_num, first_label_num);

	      fputs (integer_asm_op (4, false), asm_out_file);
	      fprint_whex (asm_out_file,
			   0x80000000
			   | (l->line_no & 0xffffff));
	      putc ('\n', asm_out_file);

	      l = next_line;
	    }

	  b = next_block;
	}

      asm_fprintf (asm_out_file, "%LLcv_lines%u_end:\n", func_num);
      func_num++;

      f = next_func;
    }
}

/* Write an entry in the S_INLINEELINES subsection of .debug$S.  */

static int
write_inlinee_lines_entry (codeview_inlinee_lines **slot,
			   void *ctx ATTRIBUTE_UNUSED)
{
  codeview_inlinee_lines *il = *slot;

  /* The inlinee lines data consists of a version uint32_t (0), followed by
     an array of struct inlinee_source_line:

      struct inlinee_source_line
      {
	  uint32_t function_id;
	  uint32_t file_id;
	  uint32_t line_no;
      };

    (see InlineeSourceLine in cvinfo.h)
  */

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, il->func_id);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, il->file_id);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, il->starting_line);
  putc ('\n', asm_out_file);

  return 1;
}

/* Write the S_INLINEELINES subsection of .debug$S, which lists the filename
   and line number for the start of each inlined function.  */

static void
write_inlinee_lines (void)
{
  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, DEBUG_S_INLINEELINES);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  asm_fprintf (asm_out_file,
	       "%LLcv_inlineelines_end - %LLcv_inlineelines_start\n");
  asm_fprintf (asm_out_file, "%LLcv_inlineelines_start:\n");

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, CV_INLINEE_SOURCE_LINE_SIGNATURE);
  putc ('\n', asm_out_file);

  inlinee_lines_htab->traverse <void*, write_inlinee_lines_entry> (NULL);

  asm_fprintf (asm_out_file, "%LLcv_inlineelines_end:\n");
}

/* Treat cold sections as separate functions, for the purposes of line
   numbers.  */

void
codeview_switch_text_section (void)
{
  codeview_function *f;

  if (cur_func && cur_func->end_label == 0)
    {
      unsigned int label_num = ++func_label_num;

      targetm.asm_out.internal_label (asm_out_file, END_FUNC_LABEL,
				      label_num);

      cur_func->end_label = label_num;
    }

  f = new_codeview_function ();
  f->parent = cur_func ? cur_func->parent : NULL;

  cur_func = f;
}

/* Mark the end of the current function.  */

void
codeview_end_epilogue (void)
{
  if (cur_func && cur_func->end_label == 0)
    {
      unsigned int label_num = ++func_label_num;

      targetm.asm_out.internal_label (asm_out_file, END_FUNC_LABEL,
				      label_num);

      cur_func->end_label = label_num;
    }
}

/* Return the CodeView constant for the selected architecture.  */

static uint16_t
target_processor (void)
{
  if (TARGET_64BIT)
    return CV_CFL_X64;
  else
    return CV_CFL_80386;
}

/* Return the CodeView constant for the language being used.  */

static uint32_t
language_constant (void)
{
  const char *language_string = lang_hooks.name;

  if (startswith (language_string, "GNU C++"))
    return CV_CFL_CXX;
  else if (startswith (language_string, "GNU C"))
    return CV_CFL_C;

  return 0;
}

/* Write a S_COMPILE3 symbol, which records the details of the compiler
   being used.  */

static void
write_compile3_symbol (void)
{
  unsigned int label_num = ++sym_label_num;

  static const char compiler_name[] = "GCC ";

  /* This is struct COMPILESYM3 in binutils and Microsoft's cvinfo.h:

     struct COMPILESYM3
     {
       uint16_t length;
       uint16_t type;
       uint32_t flags;
       uint16_t machine;
       uint16_t frontend_major;
       uint16_t frontend_minor;
       uint16_t frontend_build;
       uint16_t frontend_qfe;
       uint16_t backend_major;
       uint16_t backend_minor;
       uint16_t backend_build;
       uint16_t backend_qfe;
     } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file,
	       "%L" SYMBOL_END_LABEL "%u - %L" SYMBOL_START_LABEL "%u\n",
	       label_num, label_num);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_START_LABEL, label_num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, S_COMPILE3);
  putc ('\n', asm_out_file);

  /* Microsoft has the flags as a bitfield, with the bottom 8 bits being the
     language constant, and the reset being MSVC-specific stuff.  */
  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, language_constant ());
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, target_processor ());
  putc ('\n', asm_out_file);

  /* Write 8 uint16_ts for the frontend and backend versions.  As with GAS, we
     zero these, as it's easier to record the version in the compiler
     string.  */
  for (unsigned int i = 0; i < 8; i++)
    {
      fputs (integer_asm_op (2, false), asm_out_file);
      fprint_whex (asm_out_file, 0);
      putc ('\n', asm_out_file);
    }

  ASM_OUTPUT_ASCII (asm_out_file, compiler_name, sizeof (compiler_name) - 1);
  ASM_OUTPUT_ASCII (asm_out_file, version_string, strlen (version_string) + 1);

  ASM_OUTPUT_ALIGN (asm_out_file, 2);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_END_LABEL, label_num);
}

/* Write an S_GDATA32 symbol, representing a global variable, or an S_LDATA32
   symbol, for a static global variable.  */

static void
write_data_symbol (codeview_symbol *s)
{
  unsigned int label_num = ++sym_label_num;
  dw_attr_node *loc;
  dw_loc_descr_ref loc_ref;

  /* This is struct datasym in binutils:

      struct datasym
      {
	uint16_t size;
	uint16_t kind;
	uint32_t type;
	uint32_t offset;
	uint16_t section;
	char name[];
      } ATTRIBUTE_PACKED;
  */

  /* Extract the DW_AT_location attribute from the DIE, and make sure it's in
     in a format we can parse.  */

  loc = get_AT (s->data_symbol.die, DW_AT_location);
  if (!loc)
    goto end;

  if (loc->dw_attr_val.val_class != dw_val_class_loc)
    goto end;

  loc_ref = loc->dw_attr_val.v.val_loc;
  if (!loc_ref || loc_ref->dw_loc_opc != DW_OP_addr)
    goto end;

  /* Output the S_GDATA32 / S_LDATA32 record.  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file,
	       "%L" SYMBOL_END_LABEL "%u - %L" SYMBOL_START_LABEL "%u\n",
	       label_num, label_num);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_START_LABEL, label_num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, s->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, s->data_symbol.type);
  putc ('\n', asm_out_file);

  asm_fprintf (asm_out_file, "\t.secrel32 ");
  output_addr_const (asm_out_file, loc_ref->dw_loc_oprnd1.v.val_addr);
  fputc ('\n', asm_out_file);

  asm_fprintf (asm_out_file, "\t.secidx ");
  output_addr_const (asm_out_file, loc_ref->dw_loc_oprnd1.v.val_addr);
  fputc ('\n', asm_out_file);

  ASM_OUTPUT_ASCII (asm_out_file, s->data_symbol.name,
		    strlen (s->data_symbol.name) + 1);

  ASM_OUTPUT_ALIGN (asm_out_file, 2);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_END_LABEL, label_num);

end:
  free (s->data_symbol.name);
}

/* Write an S_LOCAL symbol, representing an optimized variable.  This is then
   followed by various S_DEFRANGE_* symbols, which describe how to find the
   value of a variable and the range for which this is valid.  */

static void
write_s_local (dw_die_ref die)
{
  unsigned int label_num = ++sym_label_num;
  const char *name = get_AT_string (die, DW_AT_name);
  uint32_t type;

  /* This is struct LOCALSYM in Microsoft's cvinfo.h:

    struct LOCALSYM {
      uint16_t reclen;
      uint16_t rectyp;
      uint32_t typind;
      uint16_t flags;
      char name[];
    };
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file,
	      "%L" SYMBOL_END_LABEL "%u - %L" SYMBOL_START_LABEL "%u\n",
	      label_num, label_num);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_START_LABEL, label_num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, S_LOCAL);
  putc ('\n', asm_out_file);

  type = get_type_num (get_AT_ref (die, DW_AT_type), false, false);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, type);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, 0);
  putc ('\n', asm_out_file);

  ASM_OUTPUT_ASCII (asm_out_file, name, strlen (name) + 1);

  ASM_OUTPUT_ALIGN (asm_out_file, 2);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_END_LABEL, label_num);
}

/* Write an S_LDATA32 symbol, representing a static variable within a function.
   This symbol can also appear outside of a function block - see
   write_data_symbol.  */

static void
write_local_s_ldata32 (dw_die_ref die, dw_loc_descr_ref loc_ref)
{
  unsigned int label_num = ++sym_label_num;
  const char *name = get_AT_string (die, DW_AT_name);
  uint32_t type;

  /* This is struct datasym in binutils:

      struct datasym
      {
	uint16_t size;
	uint16_t kind;
	uint32_t type;
	uint32_t offset;
	uint16_t section;
	char name[];
      } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file,
	       "%L" SYMBOL_END_LABEL "%u - %L" SYMBOL_START_LABEL "%u\n",
	       label_num, label_num);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_START_LABEL, label_num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, S_LDATA32);
  putc ('\n', asm_out_file);

  type = get_type_num (get_AT_ref (die, DW_AT_type), false, false);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, type);
  putc ('\n', asm_out_file);

  asm_fprintf (asm_out_file, "\t.secrel32 ");
  output_addr_const (asm_out_file, loc_ref->dw_loc_oprnd1.v.val_addr);
  fputc ('\n', asm_out_file);

  asm_fprintf (asm_out_file, "\t.secidx ");
  output_addr_const (asm_out_file, loc_ref->dw_loc_oprnd1.v.val_addr);
  fputc ('\n', asm_out_file);

  ASM_OUTPUT_ASCII (asm_out_file, name, strlen (name) + 1);

  ASM_OUTPUT_ALIGN (asm_out_file, 2);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_END_LABEL, label_num);
}

/* Try to translate a DWARF register number into its CodeView equivalent.  */

static uint16_t
dwarf_reg_to_cv (unsigned int regno)
{
  static const cv_amd64_register amd64_reg_mapping[] = {
    CV_AMD64_RAX,
    CV_AMD64_RDX,
    CV_AMD64_RCX,
    CV_AMD64_RBX,
    CV_AMD64_RSI,
    CV_AMD64_RDI,
    CV_AMD64_RBP,
    CV_AMD64_RSP,
    CV_AMD64_R8,
    CV_AMD64_R9,
    CV_AMD64_R10,
    CV_AMD64_R11,
    CV_AMD64_R12,
    CV_AMD64_R13,
    CV_AMD64_R14,
    CV_AMD64_R15,
    CV_AMD64_RIP,
    CV_AMD64_XMM0,
    CV_AMD64_XMM1,
    CV_AMD64_XMM2,
    CV_AMD64_XMM3,
    CV_AMD64_XMM4,
    CV_AMD64_XMM5,
    CV_AMD64_XMM6,
    CV_AMD64_XMM7,
    CV_AMD64_XMM8,
    CV_AMD64_XMM9,
    CV_AMD64_XMM10,
    CV_AMD64_XMM11,
    CV_AMD64_XMM12,
    CV_AMD64_XMM13,
    CV_AMD64_XMM14,
    CV_AMD64_XMM15,
    CV_AMD64_ST0,
    CV_AMD64_ST1,
    CV_AMD64_ST2,
    CV_AMD64_ST3,
    CV_AMD64_ST4,
    CV_AMD64_ST5,
    CV_AMD64_ST6,
    CV_AMD64_ST7,
    CV_AMD64_NOREG,
    CV_AMD64_NOREG,
    CV_AMD64_NOREG,
    CV_AMD64_NOREG,
    CV_AMD64_NOREG,
    CV_AMD64_NOREG,
    CV_AMD64_NOREG,
    CV_AMD64_NOREG,
    CV_AMD64_EFLAGS,
    CV_AMD64_ES,
    CV_AMD64_CS,
    CV_AMD64_SS,
    CV_AMD64_DS,
    CV_AMD64_FS,
    CV_AMD64_GS,
    CV_AMD64_NOREG,
    CV_AMD64_NOREG,
    CV_AMD64_NOREG,
    CV_AMD64_NOREG,
    CV_AMD64_NOREG,
    CV_AMD64_NOREG,
    CV_AMD64_TR,
    CV_AMD64_LDTR,
    CV_AMD64_MXCSR,
    CV_AMD64_CTRL,
    CV_AMD64_STAT
  };

  static const cv_x86_register x86_reg_mapping[] = {
    CV_REG_EAX,
    CV_REG_ECX,
    CV_REG_EDX,
    CV_REG_EBX,
    CV_REG_EBP,
    CV_REG_ESP,
    CV_REG_ESI,
    CV_REG_EDI,
    CV_REG_EIP,
    CV_REG_EFLAGS,
    CV_REG_CS,
    CV_REG_SS,
    CV_REG_DS,
    CV_REG_ES,
    CV_REG_FS,
    CV_REG_GS,
    CV_REG_ST0,
    CV_REG_ST1,
    CV_REG_ST2,
    CV_REG_ST3,
    CV_REG_ST4,
    CV_REG_ST5,
    CV_REG_ST6,
    CV_REG_ST7,
    CV_REG_CTRL,
    CV_REG_STAT,
    CV_REG_TAG,
    CV_REG_FPCS,
    CV_REG_FPIP,
    CV_REG_FPDS,
    CV_REG_FPDO,
    CV_REG_NONE,
    CV_REG_XMM0,
    CV_REG_XMM1,
    CV_REG_XMM2,
    CV_REG_XMM3,
    CV_REG_XMM4,
    CV_REG_XMM5,
    CV_REG_XMM6,
    CV_REG_XMM7,
    CV_REG_MXCSR
  };

  if (TARGET_64BIT)
    {
      if (regno < sizeof (amd64_reg_mapping) / sizeof (*amd64_reg_mapping))
	return amd64_reg_mapping[regno];

      return CV_AMD64_NOREG;
    }
  else
    {
      if (regno < sizeof (x86_reg_mapping) / sizeof (*x86_reg_mapping))
	return x86_reg_mapping[regno];

      return CV_REG_NONE;
    }
}

/* Write an S_REGISTER symbol, representing an unoptimized variable that has
   been assigned to a register.  */

static void
write_s_register (dw_die_ref die, dw_loc_descr_ref loc_ref)
{
  unsigned int label_num = ++sym_label_num;
  const char *name = get_AT_string (die, DW_AT_name);
  uint16_t regno;
  uint32_t type;

  /* This is struct regsym in binutils and REGSYM in Microsoft's cvinfo.h:

    struct regsym
    {
      uint16_t size;
      uint16_t kind;
      uint32_t type;
      uint16_t reg;
      char name[];
    } ATTRIBUTE_PACKED;
  */

  if (loc_ref->dw_loc_opc == DW_OP_regx)
    regno = dwarf_reg_to_cv (loc_ref->dw_loc_oprnd1.v.val_int);
  else
    regno = dwarf_reg_to_cv (loc_ref->dw_loc_opc - DW_OP_reg0);

  if (regno == 0)
    return;

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file,
	       "%L" SYMBOL_END_LABEL "%u - %L" SYMBOL_START_LABEL "%u\n",
	       label_num, label_num);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_START_LABEL, label_num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, S_REGISTER);
  putc ('\n', asm_out_file);

  type = get_type_num (get_AT_ref (die, DW_AT_type), false, false);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, type);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, regno);
  putc ('\n', asm_out_file);

  ASM_OUTPUT_ASCII (asm_out_file, name, strlen (name) + 1);

  ASM_OUTPUT_ALIGN (asm_out_file, 2);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_END_LABEL, label_num);
}

/* Write an S_REGREL32 symbol in order to represent an unoptimized stack
   variable.  The memory address is given by a register value plus an offset,
   so we need to parse the function's DW_AT_frame_base attribute for this.  */

static void
write_fbreg_variable (dw_die_ref die, dw_loc_descr_ref loc_ref,
		      dw_loc_descr_ref fbloc)
{
  unsigned int label_num = ++sym_label_num;
  const char *name = get_AT_string (die, DW_AT_name);
  uint32_t type;
  uint16_t regno;
  int offset;

  /* This is struct regrel in binutils and REGREL32 in Microsoft's cvinfo.h:

    struct regrel
    {
      uint16_t size;
      uint16_t kind;
      uint32_t offset;
      uint32_t type;
      uint16_t reg;
      char name[];
    } ATTRIBUTE_PACKED;
  */

  if (!fbloc)
    return;

  if (fbloc->dw_loc_opc >= DW_OP_breg0 && fbloc->dw_loc_opc <= DW_OP_breg31)
    {
      regno = dwarf_reg_to_cv (fbloc->dw_loc_opc - DW_OP_breg0);
      offset = fbloc->dw_loc_oprnd1.v.val_int;
    }
  else if (fbloc->dw_loc_opc == DW_OP_bregx)
    {
      regno = dwarf_reg_to_cv (fbloc->dw_loc_oprnd1.v.val_int);
      offset = fbloc->dw_loc_oprnd2.v.val_int;
    }
  else
    {
      return;
    }

  if (loc_ref->dw_loc_oprnd1.val_class != dw_val_class_unsigned_const)
    return;

  offset += loc_ref->dw_loc_oprnd1.v.val_int;

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file,
	       "%L" SYMBOL_END_LABEL "%u - %L" SYMBOL_START_LABEL "%u\n",
	       label_num, label_num);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_START_LABEL, label_num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, S_REGREL32);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, offset);
  putc ('\n', asm_out_file);

  type = get_type_num (get_AT_ref (die, DW_AT_type), false, false);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, type);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, regno);
  putc ('\n', asm_out_file);

  ASM_OUTPUT_ASCII (asm_out_file, name, strlen (name) + 1);

  ASM_OUTPUT_ALIGN (asm_out_file, 2);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_END_LABEL, label_num);
}

/* Write an S_DEFRANGE_REGISTER symbol, which describes a range for which an
   S_LOCAL variable is held in a certain register.  */

static void
write_defrange_register (dw_loc_descr_ref expr, rtx range_start, rtx range_end)
{
  unsigned int label_num = ++sym_label_num;
  uint16_t regno;

  /* This is defrange_register in binutils and DEFRANGESYMREGISTER in
     Microsoft's cvinfo.h:

      struct lvar_addr_range
      {
	uint32_t offset;
	uint16_t section;
	uint16_t length;
      } ATTRIBUTE_PACKED;

      struct lvar_addr_gap {
	uint16_t offset;
	uint16_t length;
      } ATTRIBUTE_PACKED;

      struct defrange_register
      {
	uint16_t size;
	uint16_t kind;
	uint16_t reg;
	uint16_t attributes;
	struct lvar_addr_range range;
	struct lvar_addr_gap gaps[];
      } ATTRIBUTE_PACKED;
  */

  if (expr->dw_loc_opc == DW_OP_regx)
    regno = dwarf_reg_to_cv (expr->dw_loc_oprnd1.v.val_int);
  else
    regno = dwarf_reg_to_cv (expr->dw_loc_opc - DW_OP_reg0);

  if (regno == 0)
    return;

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file,
	      "%L" SYMBOL_END_LABEL "%u - %L" SYMBOL_START_LABEL "%u\n",
	      label_num, label_num);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_START_LABEL, label_num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, S_DEFRANGE_REGISTER);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, regno);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, 0);
  putc ('\n', asm_out_file);

  asm_fprintf (asm_out_file, "\t.secrel32 ");
  output_addr_const (asm_out_file, range_start);
  fputc ('\n', asm_out_file);

  asm_fprintf (asm_out_file, "\t.secidx ");
  output_addr_const (asm_out_file, range_start);
  fputc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  output_addr_const (asm_out_file, range_end);
  fputs (" - ", asm_out_file);
  output_addr_const (asm_out_file, range_start);
  putc ('\n', asm_out_file);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_END_LABEL, label_num);
}

/* Write an S_DEFRANGE_REGISTER_REL symbol, which describes a range for which
   an S_LOCAL variable is held in memory given by the value of a certain
   register plus an offset.  */

static void
write_defrange_register_rel (dw_loc_descr_ref expr, dw_loc_descr_ref fbloc,
			     rtx range_start, rtx range_end)
{
  unsigned int label_num = ++sym_label_num;
  uint16_t regno;
  int offset;

  /* This is defrange_register_rel in binutils and DEFRANGESYMREGISTERREL in
     Microsoft's cvinfo.h:

      struct lvar_addr_range
      {
	uint32_t offset;
	uint16_t section;
	uint16_t length;
      } ATTRIBUTE_PACKED;

      struct lvar_addr_gap {
	uint16_t offset;
	uint16_t length;
      } ATTRIBUTE_PACKED;

      struct defrange_register_rel
      {
	uint16_t size;
	uint16_t kind;
	uint16_t reg;
	uint16_t offset_parent;
	uint32_t offset_register;
	struct lvar_addr_range range;
	struct lvar_addr_gap gaps[];
      } ATTRIBUTE_PACKED;
    */

  if (!fbloc)
    return;

  if (fbloc->dw_loc_opc >= DW_OP_breg0 && fbloc->dw_loc_opc <= DW_OP_breg31)
    {
      regno = dwarf_reg_to_cv (fbloc->dw_loc_opc - DW_OP_breg0);
      offset = fbloc->dw_loc_oprnd1.v.val_int;
    }
  else if (fbloc->dw_loc_opc == DW_OP_bregx)
    {
      regno = dwarf_reg_to_cv (fbloc->dw_loc_oprnd1.v.val_int);
      offset = fbloc->dw_loc_oprnd2.v.val_int;
    }
  else
    {
      return;
    }

  if (expr->dw_loc_oprnd1.val_class != dw_val_class_unsigned_const)
    return;

  offset += expr->dw_loc_oprnd1.v.val_int;

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file,
	      "%L" SYMBOL_END_LABEL "%u - %L" SYMBOL_START_LABEL "%u\n",
	      label_num, label_num);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_START_LABEL, label_num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, S_DEFRANGE_REGISTER_REL);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, regno);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, 0);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, offset);
  putc ('\n', asm_out_file);

  asm_fprintf (asm_out_file, "\t.secrel32 ");
  output_addr_const (asm_out_file, range_start);
  fputc ('\n', asm_out_file);

  asm_fprintf (asm_out_file, "\t.secidx ");
  output_addr_const (asm_out_file, range_start);
  fputc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  output_addr_const (asm_out_file, range_end);
  fputs (" - ", asm_out_file);
  output_addr_const (asm_out_file, range_start);
  putc ('\n', asm_out_file);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_END_LABEL, label_num);
}

/* Try to write an S_DEFRANGE_* symbol for the given DWARF location.  */

static void
write_optimized_local_variable_loc (dw_loc_descr_ref expr,
				    dw_loc_descr_ref fbloc, rtx range_start,
				    rtx range_end)
{
  if (expr->dw_loc_next)
    return;

  if (!range_start)
    return;

  if (!range_end)
    return;

  switch (expr->dw_loc_opc)
    {
    case DW_OP_reg0:
    case DW_OP_reg1:
    case DW_OP_reg2:
    case DW_OP_reg3:
    case DW_OP_reg4:
    case DW_OP_reg5:
    case DW_OP_reg6:
    case DW_OP_reg7:
    case DW_OP_reg8:
    case DW_OP_reg9:
    case DW_OP_reg10:
    case DW_OP_reg11:
    case DW_OP_reg12:
    case DW_OP_reg13:
    case DW_OP_reg14:
    case DW_OP_reg15:
    case DW_OP_reg16:
    case DW_OP_reg17:
    case DW_OP_reg18:
    case DW_OP_reg19:
    case DW_OP_reg20:
    case DW_OP_reg21:
    case DW_OP_reg22:
    case DW_OP_reg23:
    case DW_OP_reg24:
    case DW_OP_reg25:
    case DW_OP_reg26:
    case DW_OP_reg27:
    case DW_OP_reg28:
    case DW_OP_reg29:
    case DW_OP_reg30:
    case DW_OP_reg31:
    case DW_OP_regx:
      write_defrange_register (expr, range_start, range_end);
      break;

    case DW_OP_fbreg:
      write_defrange_register_rel (expr, fbloc, range_start, range_end);
      break;

    default:
      break;
    }
}

/* Write an optimized local variable, given by an S_LOCAL symbol followed by
   any number of S_DEFRANGE_* symbols.  We can't mix and match optimized and
   unoptimized variables in the same function, so even if it stays in the same
   place for the whole block we need to write an S_LOCAL.  */

static void
write_optimized_local_variable (dw_die_ref die, dw_loc_descr_ref fbloc,
				rtx block_start, rtx block_end)
{
  dw_attr_node *loc;
  dw_loc_list_ref loc_list;

  loc = get_AT (die, DW_AT_location);
  if (!loc)
    return;

  switch (loc->dw_attr_val.val_class)
    {
    case dw_val_class_loc_list:
      loc_list = loc->dw_attr_val.v.val_loc_list;

      write_s_local (die);

      while (loc_list)
	{
	  rtx range_start = NULL, range_end = NULL;

	  if (loc_list->begin)
	    range_start = gen_rtx_SYMBOL_REF (Pmode, loc_list->begin);

	  if (loc_list->end)
	    range_end = gen_rtx_SYMBOL_REF (Pmode, loc_list->end);

	  write_optimized_local_variable_loc (loc_list->expr, fbloc,
					      range_start, range_end);

	  loc_list = loc_list->dw_loc_next;
	}
      break;

    case dw_val_class_loc:
      write_s_local (die);

      write_optimized_local_variable_loc (loc->dw_attr_val.v.val_loc, fbloc,
					  block_start, block_end);
      break;

    default:
      break;
    }
}

/* Write a symbol representing an unoptimized variable within a function, if
   we're able to translate the DIE's DW_AT_location into its CodeView
   equivalent.  */

static void
write_unoptimized_local_variable (dw_die_ref die, dw_loc_descr_ref fbloc)
{
  dw_attr_node *loc;
  dw_loc_descr_ref loc_ref;

  loc = get_AT (die, DW_AT_location);
  if (!loc)
    return;

  if (loc->dw_attr_val.val_class != dw_val_class_loc)
    return;

  loc_ref = loc->dw_attr_val.v.val_loc;
  if (!loc_ref)
    return;

  switch (loc_ref->dw_loc_opc)
    {
    case DW_OP_addr:
      write_local_s_ldata32 (die, loc_ref);
      break;

    case DW_OP_reg0:
    case DW_OP_reg1:
    case DW_OP_reg2:
    case DW_OP_reg3:
    case DW_OP_reg4:
    case DW_OP_reg5:
    case DW_OP_reg6:
    case DW_OP_reg7:
    case DW_OP_reg8:
    case DW_OP_reg9:
    case DW_OP_reg10:
    case DW_OP_reg11:
    case DW_OP_reg12:
    case DW_OP_reg13:
    case DW_OP_reg14:
    case DW_OP_reg15:
    case DW_OP_reg16:
    case DW_OP_reg17:
    case DW_OP_reg18:
    case DW_OP_reg19:
    case DW_OP_reg20:
    case DW_OP_reg21:
    case DW_OP_reg22:
    case DW_OP_reg23:
    case DW_OP_reg24:
    case DW_OP_reg25:
    case DW_OP_reg26:
    case DW_OP_reg27:
    case DW_OP_reg28:
    case DW_OP_reg29:
    case DW_OP_reg30:
    case DW_OP_reg31:
    case DW_OP_regx:
      write_s_register (die, loc_ref);
      break;

    case DW_OP_fbreg:
      write_fbreg_variable (die, loc_ref, fbloc);
      break;

    default:
      break;
    }
}

/* Translate a DW_TAG_lexical_block DIE into an S_BLOCK32 symbol, representing
   a block within an unoptimized function.  Returns false if we're not able
   to resolve the location, which will prevent the caller from issuing an
   unneeded S_END.  */

static bool
write_s_block32 (dw_die_ref die)
{
  unsigned int label_num = ++sym_label_num;
  dw_attr_node *loc_low, *loc_high;
  const char *label_low, *label_high;
  rtx rtx_low, rtx_high;

  /* This is struct blocksym in binutils and BLOCKSYM32 in Microsoft's
     cvinfo.h:

    struct blocksym
    {
      uint16_t size;
      uint16_t kind;
      uint32_t parent;
      uint32_t end;
      uint32_t len;
      uint32_t offset;
      uint16_t section;
      char name[];
    } ATTRIBUTE_PACKED;
  */

  loc_low = get_AT (die, DW_AT_low_pc);
  if (!loc_low)
    return false;

  if (loc_low->dw_attr_val.val_class != dw_val_class_lbl_id)
    return false;

  label_low = loc_low->dw_attr_val.v.val_lbl_id;
  if (!label_low)
    return false;

  rtx_low = gen_rtx_SYMBOL_REF (Pmode, label_low);

  loc_high = get_AT (die, DW_AT_high_pc);
  if (!loc_high)
    return false;

  if (loc_high->dw_attr_val.val_class != dw_val_class_high_pc)
    return false;

  label_high = loc_high->dw_attr_val.v.val_lbl_id;
  if (!label_high)
    return false;

  rtx_high = gen_rtx_SYMBOL_REF (Pmode, label_high);

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file,
	       "%L" SYMBOL_END_LABEL "%u - %L" SYMBOL_START_LABEL "%u\n",
	       label_num, label_num);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_START_LABEL, label_num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, S_BLOCK32);
  putc ('\n', asm_out_file);

  /* The parent and end fields get filled in by the linker.  */

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, 0);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, 0);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  output_addr_const (asm_out_file, rtx_high);
  fputs (" - ", asm_out_file);
  output_addr_const (asm_out_file, rtx_low);
  putc ('\n', asm_out_file);

  asm_fprintf (asm_out_file, "\t.secrel32 ");
  output_addr_const (asm_out_file, rtx_low);
  fputc ('\n', asm_out_file);

  asm_fprintf (asm_out_file, "\t.secidx ");
  output_addr_const (asm_out_file, rtx_low);
  fputc ('\n', asm_out_file);

  ASM_OUTPUT_ASCII (asm_out_file, "", 1);

  ASM_OUTPUT_ALIGN (asm_out_file, 2);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_END_LABEL, label_num);

  return true;
}

/* Write an S_END symbol, which is used to finish off a number of different
   symbol types.  Here we use it to mark the S_BLOCK32 as finished.  */

static void
write_s_end (void)
{
  unsigned int label_num = ++sym_label_num;

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file,
	       "%L" SYMBOL_END_LABEL "%u - %L" SYMBOL_START_LABEL "%u\n",
	       label_num, label_num);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_START_LABEL, label_num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, S_END);
  putc ('\n', asm_out_file);

  ASM_OUTPUT_ALIGN (asm_out_file, 2);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_END_LABEL, label_num);
}

/* Write the S_FRAMEPROC symbol, which is supposed to give information about
   the function frame.  It doesn't seem to be really used in modern versions of
   MSVC, which is why we zero-out everything here.  You still need to write it
   though, otherwise windbg won't necessarily show all the local variables.  */

static void
write_s_frameproc (void)
{
  unsigned int label_num = ++sym_label_num;

  /* This is struct FRAMEPROCSYM in Microsoft's cvinfo.h:

      struct frameprocsym
      {
	uint16_t size;
	uint16_t kind;
	uint32_t frame_size;
	uint32_t padding_size;
	uint32_t padding_offset;
	uint32_t saved_registers_size;
	uint32_t exception_handler_offset;
	uint16_t exception_handler_section;
	uint32_t flags;
      } ATTRIBUTE_PACKED;
   */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file,
	       "%L" SYMBOL_END_LABEL "%u - %L" SYMBOL_START_LABEL "%u\n",
	       label_num, label_num);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_START_LABEL, label_num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, S_FRAMEPROC);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, 0);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, 0);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, 0);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, 0);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, 0);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, 0);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, 0);
  putc ('\n', asm_out_file);

  ASM_OUTPUT_ALIGN (asm_out_file, 2);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_END_LABEL, label_num);
}

/* Loop through the DIEs in an unoptimized function, writing out any variables
   or blocks that we encounter.  */

static void
write_unoptimized_function_vars (dw_die_ref die, dw_loc_descr_ref fbloc)
{
  dw_die_ref first_child, c;

  first_child = dw_get_die_child (die);

  if (!first_child)
    return;

  c = first_child;
  do
  {
    c = dw_get_die_sib (c);

    switch (dw_get_die_tag (c))
      {
      case DW_TAG_formal_parameter:
      case DW_TAG_variable:
	write_unoptimized_local_variable (c, fbloc);
	break;

      case DW_TAG_lexical_block:
	{
	  bool block_started = write_s_block32 (c);

	  write_unoptimized_function_vars (c, fbloc);

	  if (block_started)
	    write_s_end ();

	  break;
	}

      default:
	break;
      }
  }
  while (c != first_child);
}

/* Write the variables in an optimized function or block.  There's no S_BLOCK32s
   here, with the range determining the lifetime of a variable.  Unfortunately
   for us CodeView is much less expressive than DWARF when it comes to variable
   locations, so some degree of "optimized out"s is inevitable.  */

static void
write_optimized_function_vars (dw_die_ref die, dw_loc_descr_ref fbloc,
			       rtx block_start, rtx block_end)
{
  dw_die_ref first_child, c;

  first_child = dw_get_die_child (die);

  if (!first_child)
    return;

  c = first_child;
  do
  {
    c = dw_get_die_sib (c);

    switch (dw_get_die_tag (c))
      {
      case DW_TAG_formal_parameter:
      case DW_TAG_variable:
	write_optimized_local_variable (c, fbloc, block_start, block_end);
	break;

      case DW_TAG_lexical_block:
	{
	  dw_attr_node *loc_low, *loc_high;
	  const char *label_low, *label_high;
	  rtx rtx_low, rtx_high;

	  loc_low = get_AT (die, DW_AT_low_pc);
	  if (!loc_low)
	    break;

	  if (loc_low->dw_attr_val.val_class != dw_val_class_lbl_id)
	    break;

	  label_low = loc_low->dw_attr_val.v.val_lbl_id;
	  if (!label_low)
	    break;

	  rtx_low = gen_rtx_SYMBOL_REF (Pmode, label_low);

	  loc_high = get_AT (die, DW_AT_high_pc);
	  if (!loc_high)
	    break;

	  if (loc_high->dw_attr_val.val_class != dw_val_class_high_pc)
	    break;

	  label_high = loc_high->dw_attr_val.v.val_lbl_id;
	  if (!label_high)
	    break;

	  rtx_high = gen_rtx_SYMBOL_REF (Pmode, label_high);

	  write_optimized_function_vars (c, fbloc, rtx_low, rtx_high);

	  break;
	}

      default:
	break;
      }
  }
  while (c != first_child);
}

/* There's no way to mark the range of a static local variable in an optimized
   function: there's no S_DEFRANGE_* symbol for this, and you can't have
   S_BLOCK32 symbols.  So instead we have to loop through after the S_FRAMEPROC
   has been written, and write the S_LDATA32s at the end.  */

static void
write_optimized_static_local_vars (dw_die_ref die)
{
  dw_die_ref first_child, c;

  first_child = dw_get_die_child (die);

  if (!first_child)
    return;

  c = first_child;
  do
  {
    c = dw_get_die_sib (c);

    switch (dw_get_die_tag (c))
      {
      case DW_TAG_variable:
	{
	  dw_attr_node *loc;
	  dw_loc_descr_ref loc_ref;

	  loc = get_AT (c, DW_AT_location);
	  if (!loc)
	    break;

	  if (loc->dw_attr_val.val_class != dw_val_class_loc)
	    break;

	  loc_ref = loc->dw_attr_val.v.val_loc;
	  if (!loc_ref)
	    break;

	  if (loc_ref->dw_loc_opc != DW_OP_addr)
	    break;

	  write_local_s_ldata32 (c, loc_ref);
	  break;
	}

      case DW_TAG_lexical_block:
	write_optimized_static_local_vars (c);
	break;

      default:
	break;
      }
  }
  while (c != first_child);
}

#ifdef HAVE_GAS_CV_UCOMP

/* Given a DW_TAG_inlined_subroutine DIE within parent_func, return a pointer
   to the corresponding codeview_function, which is used to map addresses
   to line numbers.  */

static codeview_function *
find_line_function (dw_die_ref parent_func, dw_die_ref die)
{
  codeview_function **slot, *f;

  if (!cv_func_htab)
    return NULL;

  slot = cv_func_htab->find_slot_with_hash (parent_func,
					    htab_hash_pointer (parent_func),
					    NO_INSERT);

  if (!slot || !*slot)
    return NULL;

  f = *slot;

  while (f)
    {
      expanded_location loc;
      dwarf_file_data *call_file;

      if (f->inline_block == 0)
	{
	  f = f->htab_next;
	  continue;
	}

      loc = expand_location (f->inline_loc);

      if ((unsigned) loc.line != get_AT_unsigned (die, DW_AT_call_line)
	  || (unsigned) loc.column != get_AT_unsigned (die, DW_AT_call_column))
	{
	  f = f->htab_next;
	  continue;
	}

      call_file = get_AT_file (die, DW_AT_call_file);

      if (!call_file || strcmp (call_file->filename, loc.file))
	{
	  f = f->htab_next;
	  continue;
	}

      return f;
    }

  return NULL;
}

/* Write the "binary annotations" for an S_INLINESITE symbol, which are how
   CodeView represents line numbers within inlined functions.  This is
   completely different to how line numbers are represented normally, and
   requires assembler support for the .cv_ucomp and .cv_scomp pseudos.  */

static void
write_binary_annotations (codeview_function *line_func, uint32_t func_id)
{
  codeview_line_block *b;
  codeview_inlinee_lines **slot, *il;
  codeview_function *top_parent;
  unsigned int line_no, label_num;

  slot = inlinee_lines_htab->find_slot_with_hash (func_id, func_id, NO_INSERT);
  if (!slot || !*slot)
    return;

  il = *slot;

  line_no = il->starting_line;

  top_parent = line_func;
  while (top_parent->parent)
    {
      top_parent = top_parent->parent;
    }

  label_num = top_parent->blocks->lines->label_num;

  b = line_func->blocks;
  while (b)
    {
      codeview_line *l = b->lines;

      while (l)
	{
	  if (!l->next && !b->next) /* last line (end of block) */
	    {
	      asm_fprintf (asm_out_file, "\t.cv_ucomp %u\n",
		       ba_op_change_code_length);
	      asm_fprintf (asm_out_file,
		       "\t.cv_ucomp %L" LINE_LABEL "%u - %L" LINE_LABEL "%u\n",
		       l->label_num, label_num);
	    }
	  else
	    {
	      if (l->line_no != line_no)
		{
		  asm_fprintf (asm_out_file, "\t.cv_ucomp %u\n",
			       ba_op_change_line_offset);
		  asm_fprintf (asm_out_file, "\t.cv_scomp %i\n",
			       l->line_no - line_no);

		  line_no = l->line_no;
		}

	      asm_fprintf (asm_out_file, "\t.cv_ucomp %u\n",
			   ba_op_change_code_offset);
	      asm_fprintf (asm_out_file,
			"\t.cv_ucomp %L" LINE_LABEL "%u - %L" LINE_LABEL "%u\n",
			l->label_num, label_num);
	    }

	  label_num = l->label_num;

	  l = l->next;
	}

      b = b->next;
    }
}

#endif

/* Write an S_INLINESITE symbol, to record that a function has been inlined
   inside another function.  */

static void
write_s_inlinesite (dw_die_ref parent_func, dw_die_ref die)
{
  unsigned int label_num = ++sym_label_num;
  dw_die_ref func;
  uint32_t func_id;
  codeview_function *line_func;

  func = get_AT_ref (die, DW_AT_abstract_origin);
  if (!func)
    return;

  func_id = get_func_id (func);
  if (func_id == 0)
    return;

  /* This is struct inline_site in binutils and INLINESITESYM in Microsoft's
     cvinfo.h:

      struct inline_site
      {
	uint16_t size;
	uint16_t kind;
	uint32_t parent;
	uint32_t end;
	uint32_t inlinee;
	uint8_t binary_annotations[];
      } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file,
	       "%L" SYMBOL_END_LABEL "%u - %L" SYMBOL_START_LABEL "%u\n",
	       label_num, label_num);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_START_LABEL, label_num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, S_INLINESITE);
  putc ('\n', asm_out_file);

  /* parent, filled in by linker */
  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, 0);
  putc ('\n', asm_out_file);

  /* end, filled in by linker */
  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, 0);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, func_id);
  putc ('\n', asm_out_file);

#ifdef HAVE_GAS_CV_UCOMP
  line_func = find_line_function (parent_func, die);

  if (line_func)
    {
      write_binary_annotations (line_func, func_id);
      ASM_OUTPUT_ALIGN (asm_out_file, 2);
    }
#else
  (void) line_func;
#endif

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_END_LABEL, label_num);

  write_inlinesite_records (parent_func, die);

  /* Write S_INLINESITE_END symbol.  */

  label_num = ++sym_label_num;

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file,
	       "%L" SYMBOL_END_LABEL "%u - %L" SYMBOL_START_LABEL "%u\n",
	       label_num, label_num);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_START_LABEL, label_num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, S_INLINESITE_END);
  putc ('\n', asm_out_file);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_END_LABEL, label_num);
}

/* Loop through a function, and translate its DW_TAG_inlined_subroutine DIEs
   into CodeView S_INLINESITE symbols.  */

static void
write_inlinesite_records (dw_die_ref func, dw_die_ref die)
{
  dw_die_ref first_child, c;

  first_child = dw_get_die_child (die);

  if (!first_child)
    return;

  c = first_child;
  do
    {
      c = dw_get_die_sib (c);

      switch (dw_get_die_tag (c))
	{
	case DW_TAG_lexical_block:
	  write_inlinesite_records (func, c);
	  break;

	case DW_TAG_inlined_subroutine:
	  write_s_inlinesite (func, c);
	  break;

	default:
	  break;
	}
    }
  while (c != first_child);
}

/* Write an S_GPROC32_ID symbol, representing a global function, or an
   S_LPROC32_ID symbol, for a static function.  */

static void
write_function (codeview_symbol *s)
{
  unsigned int label_num = ++sym_label_num;
  dw_attr_node *loc_low, *loc_high, *frame_base;
  const char *label_low, *label_high;
  rtx rtx_low, rtx_high;
  dw_loc_descr_ref fbloc = NULL;

  /* This is struct procsym in binutils and PROCSYM32 in Microsoft's cvinfo.h:

      struct procsym
      {
	uint16_t size;
	uint16_t kind;
	uint32_t parent;
	uint32_t end;
	uint32_t next;
	uint32_t proc_len;
	uint32_t debug_start;
	uint32_t debug_end;
	uint32_t type;
	uint32_t offset;
	uint16_t section;
	uint8_t flags;
	char name[];
      } ATTRIBUTE_PACKED;
  */

  loc_low = get_AT (s->function.die, DW_AT_low_pc);
  if (!loc_low)
    goto end;

  if (loc_low->dw_attr_val.val_class != dw_val_class_lbl_id)
    goto end;

  label_low = loc_low->dw_attr_val.v.val_lbl_id;
  if (!label_low)
    goto end;

  rtx_low = gen_rtx_SYMBOL_REF (Pmode, label_low);

  loc_high = get_AT (s->function.die, DW_AT_high_pc);
  if (!loc_high)
    goto end;

  if (loc_high->dw_attr_val.val_class != dw_val_class_high_pc)
    goto end;

  label_high = loc_high->dw_attr_val.v.val_lbl_id;
  if (!label_high)
    goto end;

  rtx_high = gen_rtx_SYMBOL_REF (Pmode, label_high);

  /* Output the S_GPROC32_ID / S_LPROC32_ID record.  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file,
	       "%L" SYMBOL_END_LABEL "%u - %L" SYMBOL_START_LABEL "%u\n",
	       label_num, label_num);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_START_LABEL, label_num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, s->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, s->function.parent);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, s->function.end);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, s->function.next);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  output_addr_const (asm_out_file, rtx_high);
  fputs (" - ", asm_out_file);
  output_addr_const (asm_out_file, rtx_low);
  putc ('\n', asm_out_file);

  /* FIXME - debug_start should be the end of the prologue, and debug_end
	     the beginning of the epilogue.  Do the whole function for
	     now.  */

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, 0);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  output_addr_const (asm_out_file, rtx_high);
  fputs (" - ", asm_out_file);
  output_addr_const (asm_out_file, rtx_low);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, s->function.type);
  putc ('\n', asm_out_file);

  asm_fprintf (asm_out_file, "\t.secrel32 ");
  output_addr_const (asm_out_file, rtx_low);
  fputc ('\n', asm_out_file);

  asm_fprintf (asm_out_file, "\t.secidx ");
  output_addr_const (asm_out_file, rtx_low);
  fputc ('\n', asm_out_file);

  fputs (integer_asm_op (1, false), asm_out_file);
  fprint_whex (asm_out_file, s->function.flags);
  putc ('\n', asm_out_file);

  ASM_OUTPUT_ASCII (asm_out_file, s->function.name,
		    strlen (s->function.name) + 1);

  ASM_OUTPUT_ALIGN (asm_out_file, 2);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_END_LABEL, label_num);

  write_inlinesite_records (s->function.die, s->function.die);

  frame_base = get_AT (s->function.die, DW_AT_frame_base);

  if (frame_base && frame_base->dw_attr_val.val_class == dw_val_class_loc)
    fbloc = frame_base->dw_attr_val.v.val_loc;

  if (flag_var_tracking)
    {
      write_optimized_function_vars (s->function.die, fbloc, rtx_low,
				     rtx_high);
      write_s_frameproc ();
      write_optimized_static_local_vars (s->function.die);
    }
  else
    {
      write_s_frameproc ();
      write_unoptimized_function_vars (s->function.die, fbloc);
    }

  /* Output the S_PROC_ID_END record.  */

  label_num = ++sym_label_num;

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file,
	       "%L" SYMBOL_END_LABEL "%u - %L" SYMBOL_START_LABEL "%u\n",
	       label_num, label_num);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_START_LABEL, label_num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, S_PROC_ID_END);
  putc ('\n', asm_out_file);

  targetm.asm_out.internal_label (asm_out_file, SYMBOL_END_LABEL, label_num);

end:
  free (s->function.name);
}

/* Write the CodeView symbols into the .debug$S section.  */

static void
write_codeview_symbols (void)
{
  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, DEBUG_S_SYMBOLS);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_syms_end - %LLcv_syms_start\n");

  asm_fprintf (asm_out_file, "%LLcv_syms_start:\n");

  write_compile3_symbol ();

  while (sym)
    {
      codeview_symbol *n = sym->next;

      switch (sym->kind)
	{
	case S_LDATA32:
	case S_GDATA32:
	  write_data_symbol (sym);
	  break;
	case S_LPROC32_ID:
	case S_GPROC32_ID:
	  write_function (sym);
	  break;
	default:
	  break;
	}

      free (sym);
      sym = n;
    }

  asm_fprintf (asm_out_file, "%LLcv_syms_end:\n");
}

/* Write an LF_POINTER type.  */

static void
write_lf_pointer (codeview_custom_type *t)
{
  /* This is lf_pointer in binutils and lfPointer in Microsoft's cvinfo.h:

    struct lf_pointer
    {
      uint16_t size;
      uint16_t kind;
      uint32_t base_type;
      uint32_t attributes;
      (following only if CV_PTR_MODE_PMEM or CV_PTR_MODE_PMFUNC in attributes)
      uint32_t containing_class;
      uint16_t ptr_to_mem_type;
      uint16_t padding;
    } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_pointer.base_type);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_pointer.attributes);
  putc ('\n', asm_out_file);

  if ((t->lf_pointer.attributes & CV_PTR_MODE_MASK) == CV_PTR_MODE_PMEM
      || (t->lf_pointer.attributes & CV_PTR_MODE_MASK) == CV_PTR_MODE_PMFUNC)
    {
      fputs (integer_asm_op (4, false), asm_out_file);
      fprint_whex (asm_out_file, t->lf_pointer.containing_class);
      putc ('\n', asm_out_file);

      fputs (integer_asm_op (2, false), asm_out_file);
      fprint_whex (asm_out_file, t->lf_pointer.ptr_to_mem_type);
      putc ('\n', asm_out_file);

      write_cv_padding (2);
    }

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* All CodeView type definitions have to be aligned to a four-byte boundary,
   so write some padding bytes if necessary.  These have to be specific values:
   LF_PAD3, LF_PAD2, LF_PAD1.  */

static void
write_cv_padding (size_t padding)
{
  if (padding == 4 || padding == 0)
    return;

  if (padding == 3)
    {
      fputs (integer_asm_op (1, false), asm_out_file);
      fprint_whex (asm_out_file, LF_PAD3);
      putc ('\n', asm_out_file);
    }

  if (padding >= 2)
    {
      fputs (integer_asm_op (1, false), asm_out_file);
      fprint_whex (asm_out_file, LF_PAD2);
      putc ('\n', asm_out_file);
    }

  fputs (integer_asm_op (1, false), asm_out_file);
  fprint_whex (asm_out_file, LF_PAD1);
  putc ('\n', asm_out_file);
}

/* Write an LF_MODIFIER type, representing a const and/or volatile modification
   of another type.  */

static void
write_lf_modifier (codeview_custom_type *t)
{
  /* This is lf_modifier in binutils and lfModifier in Microsoft's cvinfo.h:

    struct lf_modifier
    {
      uint16_t size;
      uint16_t kind;
      uint32_t base_type;
      uint16_t modifier;
      uint16_t padding;
    } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_modifier.base_type);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_modifier.modifier);
  putc ('\n', asm_out_file);

  write_cv_padding (2);

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write a CodeView extensible integer.  If the value is non-negative and
   < 0x8000, the value gets written directly as an uint16_t.  Otherwise, we
   output two bytes for the integer type (LF_CHAR, LF_SHORT, ...), and the
   actual value follows.  Returns the total number of bytes written.  */

static size_t
write_cv_integer (codeview_integer *i)
{
  if (i->neg)
    {
      if (i->num <= 0x80)
	{
	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_CHAR);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (1, false), asm_out_file);
	  fprint_whex (asm_out_file, -i->num);
	  putc ('\n', asm_out_file);

	  return 3;
	}
      else if (i->num <= 0x8000)
	{
	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_SHORT);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, -i->num);
	  putc ('\n', asm_out_file);

	  return 4;
	}
      else if (i->num <= 0x80000000)
	{
	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_LONG);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (4, false), asm_out_file);
	  fprint_whex (asm_out_file, -i->num);
	  putc ('\n', asm_out_file);

	  return 6;
	}
      else
	{
	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_QUADWORD);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (8, false), asm_out_file);
	  fprint_whex (asm_out_file, -i->num);
	  putc ('\n', asm_out_file);

	  return 10;
	}
    }
  else
    {
      if (i->num <= 0x7fff)
	{
	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, i->num);
	  putc ('\n', asm_out_file);

	  return 2;
	}
      else if (i->num <= 0xffff)
	{
	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_USHORT);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, i->num);
	  putc ('\n', asm_out_file);

	  return 4;
	}
      else if (i->num <= 0xffffffff)
	{
	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_ULONG);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (4, false), asm_out_file);
	  fprint_whex (asm_out_file, i->num);
	  putc ('\n', asm_out_file);

	  return 6;
	}
      else
	{
	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_UQUADWORD);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (8, false), asm_out_file);
	  fprint_whex (asm_out_file, i->num);
	  putc ('\n', asm_out_file);

	  return 10;
	}
    }
}

/* Return the extra size needed for an extensible integer.  */

static size_t
cv_integer_len (codeview_integer *i)
{
  if (i->neg)
    {
      if (i->num <= 0x80)
	return sizeof (int8_t);
      else if (i->num <= 0x8000)
	return sizeof (int16_t);
      else if (i->num <= 0x80000000)
	return sizeof (int32_t);
      else
	return sizeof (int64_t);
    }
  else
    {
      if (i->num <= 0x7fff)
	return 0;
      else if (i->num <= 0xffff)
	return sizeof (uint16_t);
      else if (i->num <= 0xffffffff)
	return sizeof (uint32_t);
      else
	return sizeof (uint64_t);
    }
}

/* Write an LF_FIELDLIST type, which is a container for various subtypes.  This
   has two uses: for the values in an enum, and for the member, operators etc.
   for a struct, class, or union.  */

static void
write_lf_fieldlist (codeview_custom_type *t)
{
  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  while (t->lf_fieldlist.subtypes)
    {
      codeview_subtype *v = t->lf_fieldlist.subtypes;
      codeview_subtype *next = v->next;
      size_t name_len, leaf_len;

      switch (v->kind)
	{
	case LF_ENUMERATE:
	  /* This is lf_enumerate in binutils and lfEnumerate in Microsoft's
	     cvinfo.h:

	    struct lf_enumerate
	    {
	      uint16_t kind;
	      uint16_t attributes;
	      uint16_t value;
	      (then actual value if value >= 0x8000)
	      char name[];
	    } ATTRIBUTE_PACKED;
	  */

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_ENUMERATE);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, CV_ACCESS_PUBLIC);
	  putc ('\n', asm_out_file);

	  leaf_len = 4 + write_cv_integer (&v->lf_enumerate.value);

	  name_len = strlen (v->lf_enumerate.name) + 1;
	  ASM_OUTPUT_ASCII (asm_out_file, v->lf_enumerate.name, name_len);

	  leaf_len += name_len;
	  write_cv_padding (4 - (leaf_len % 4));

	  free (v->lf_enumerate.name);
	  break;

	case LF_MEMBER:
	  /* This is lf_member in binutils and lfMember in Microsoft's
	     cvinfo.h:

	    struct lf_member
	    {
	      uint16_t kind;
	      uint16_t attributes;
	      uint32_t type;
	      uint16_t offset;
	      char name[];
	    } ATTRIBUTE_PACKED;
	  */

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_MEMBER);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, v->lf_member.attributes);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (4, false), asm_out_file);
	  fprint_whex (asm_out_file, v->lf_member.type);
	  putc ('\n', asm_out_file);

	  leaf_len = 8 + write_cv_integer (&v->lf_member.offset);

	  if (v->lf_member.name)
	    {
	      name_len = strlen (v->lf_member.name) + 1;
	      ASM_OUTPUT_ASCII (asm_out_file, v->lf_member.name, name_len);
	    }
	  else
	    {
	      name_len = 1;
	      ASM_OUTPUT_ASCII (asm_out_file, "", name_len);
	    }

	  leaf_len += name_len;
	  write_cv_padding (4 - (leaf_len % 4));

	  free (v->lf_member.name);
	  break;

	case LF_INDEX:
	  /* This is lf_index in binutils and lfIndex in Microsoft's cvinfo.h:

	    struct lf_index
	    {
	      uint16_t kind;
	      uint16_t padding;
	      uint32_t index;
	    } ATTRIBUTE_PACKED;
	  */

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_INDEX);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, 0);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (4, false), asm_out_file);
	  fprint_whex (asm_out_file, v->lf_index.type_num);
	  putc ('\n', asm_out_file);

	  break;

	case LF_STMEMBER:
	  /* This is lf_static_member in binutils and lfSTMember in Microsoft's
	     cvinfo.h:

	    struct lf_static_member
	    {
	      uint16_t kind;
	      uint16_t attributes;
	      uint32_t type;
	      char name[];
	    } ATTRIBUTE_PACKED;
	  */

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_STMEMBER);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, v->lf_static_member.attributes);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (4, false), asm_out_file);
	  fprint_whex (asm_out_file, v->lf_static_member.type);
	  putc ('\n', asm_out_file);

	  name_len = strlen (v->lf_static_member.name) + 1;
	  ASM_OUTPUT_ASCII (asm_out_file, v->lf_static_member.name, name_len);

	  leaf_len = 8 + name_len;
	  write_cv_padding (4 - (leaf_len % 4));

	  free (v->lf_static_member.name);
	  break;

	case LF_ONEMETHOD:
	  /* This is lf_onemethod in binutils and lfOneMethod in Microsoft's
	     cvinfo.h:

	    struct lf_onemethod
	    {
	      uint16_t kind;
	      uint16_t method_attribute;
	      uint32_t method_type;
	      char name[];
	    } ATTRIBUTE_PACKED;
	  */

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_ONEMETHOD);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, v->lf_onemethod.method_attribute);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (4, false), asm_out_file);
	  fprint_whex (asm_out_file, v->lf_onemethod.method_type);
	  putc ('\n', asm_out_file);

	  name_len = strlen (v->lf_onemethod.name) + 1;
	  ASM_OUTPUT_ASCII (asm_out_file, v->lf_onemethod.name, name_len);

	  leaf_len = 8 + name_len;
	  write_cv_padding (4 - (leaf_len % 4));

	  free (v->lf_onemethod.name);
	  break;

	case LF_METHOD:
	  /* This is lf_method in binutils and lfMethod in Microsoft's
	     cvinfo.h:

	    struct lf_method
	    {
	      uint16_t kind;
	      uint16_t count;
	      uint32_t method_list;
	      char name[];
	    } ATTRIBUTE_PACKED;
	  */

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_METHOD);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, v->lf_method.count);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (4, false), asm_out_file);
	  fprint_whex (asm_out_file, v->lf_method.method_list);
	  putc ('\n', asm_out_file);

	  name_len = strlen (v->lf_method.name) + 1;
	  ASM_OUTPUT_ASCII (asm_out_file, v->lf_method.name, name_len);

	  leaf_len = 8 + name_len;
	  write_cv_padding (4 - (leaf_len % 4));

	  free (v->lf_method.name);
	  break;

	case LF_BCLASS:
	  /* This is lf_bclass in binutils and lfBClass in Microsoft's
	     cvinfo.h:

	    struct lf_bclass
	    {
	      uint16_t kind;
	      uint16_t attributes;
	      uint32_t base_class_type;
	      uint16_t offset;
	    } ATTRIBUTE_PACKED;
	  */

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_BCLASS);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, v->lf_bclass.attributes);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (4, false), asm_out_file);
	  fprint_whex (asm_out_file, v->lf_bclass.base_class_type);
	  putc ('\n', asm_out_file);

	  leaf_len = 8 + write_cv_integer (&v->lf_bclass.offset);

	  write_cv_padding (4 - (leaf_len % 4));
	  break;

	case LF_NESTTYPE:
	  /* This is lf_nest_type in binutils and lfNestType in Microsoft's
	     cvinfo.h:

	    struct lf_nest_type
	    {
	      uint16_t kind;
	      uint16_t padding;
	      uint32_t type;
	      char name[];
	    } ATTRIBUTE_PACKED;
	  */

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, LF_NESTTYPE);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (2, false), asm_out_file);
	  fprint_whex (asm_out_file, 0);
	  putc ('\n', asm_out_file);

	  fputs (integer_asm_op (4, false), asm_out_file);
	  fprint_whex (asm_out_file, v->lf_nesttype.type);
	  putc ('\n', asm_out_file);

	  name_len = strlen (v->lf_nesttype.name) + 1;
	  ASM_OUTPUT_ASCII (asm_out_file, v->lf_nesttype.name, name_len);

	  leaf_len = 8 + name_len;
	  write_cv_padding (4 - (leaf_len % 4));

	  free (v->lf_nesttype.name);
	  break;

	default:
	  break;
	}

      t->lf_fieldlist.subtypes = next;
      free (v);
    }

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write an LF_ENUM type.  */

static void
write_lf_enum (codeview_custom_type *t)
{
  size_t name_len, leaf_len;

  /* This is lf_enum in binutils and lfEnum in Microsoft's cvinfo.h:

    struct lf_enum
    {
      uint16_t size;
      uint16_t kind;
      uint16_t num_elements;
      uint16_t properties;
      uint32_t underlying_type;
      uint32_t field_list;
      char name[];
    } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_enum.count);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_enum.properties);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_enum.underlying_type);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_enum.fieldlist);
  putc ('\n', asm_out_file);

  name_len = strlen (t->lf_enum.name) + 1;
  ASM_OUTPUT_ASCII (asm_out_file, t->lf_enum.name, name_len);

  leaf_len = 14 + name_len;
  write_cv_padding (4 - (leaf_len % 4));

  free (t->lf_enum.name);

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write an LF_STRUCTURE or LF_CLASS type (the two have the same structure).  */

static void
write_lf_structure (codeview_custom_type *t)
{
  size_t name_len, leaf_len;

  /* This is lf_class in binutils and lfClass in Microsoft's cvinfo.h:

    struct lf_class
    {
      uint16_t size;
      uint16_t kind;
      uint16_t num_members;
      uint16_t properties;
      uint32_t field_list;
      uint32_t derived_from;
      uint32_t vshape;
      uint16_t length;
      char name[];
    } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_structure.num_members);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_structure.properties);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_structure.field_list);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_structure.derived_from);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_structure.vshape);
  putc ('\n', asm_out_file);

  leaf_len = 20 + write_cv_integer (&t->lf_structure.length);

  if (t->lf_structure.name)
    {
      name_len = strlen (t->lf_structure.name) + 1;
      ASM_OUTPUT_ASCII (asm_out_file, t->lf_structure.name, name_len);
    }
  else
    {
      static const char unnamed_struct[] = "<unnamed-tag>";

      name_len = sizeof (unnamed_struct);
      ASM_OUTPUT_ASCII (asm_out_file, unnamed_struct, name_len);
    }

  leaf_len += name_len;
  write_cv_padding (4 - (leaf_len % 4));

  free (t->lf_structure.name);

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write an LF_UNION type.  */

static void
write_lf_union (codeview_custom_type *t)
{
  size_t name_len, leaf_len;

  /* This is lf_union in binutils and lfUnion in Microsoft's cvinfo.h:

    struct lf_union
    {
      uint16_t size;
      uint16_t kind;
      uint16_t num_members;
      uint16_t properties;
      uint32_t field_list;
      uint16_t length;
      char name[];
    } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_structure.num_members);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_structure.properties);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_structure.field_list);
  putc ('\n', asm_out_file);

  leaf_len = 12 + write_cv_integer (&t->lf_structure.length);

  if (t->lf_structure.name)
    {
      name_len = strlen (t->lf_structure.name) + 1;
      ASM_OUTPUT_ASCII (asm_out_file, t->lf_structure.name, name_len);
    }
  else
    {
      static const char unnamed_struct[] = "<unnamed-tag>";

      name_len = sizeof (unnamed_struct);
      ASM_OUTPUT_ASCII (asm_out_file, unnamed_struct, name_len);
    }

  leaf_len += name_len;
  write_cv_padding (4 - (leaf_len % 4));

  free (t->lf_structure.name);

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write an LF_ARRAY type.  */

static void
write_lf_array (codeview_custom_type *t)
{
  size_t leaf_len;

  /* This is lf_array in binutils and lfArray in Microsoft's cvinfo.h:

    struct lf_array
    {
      uint16_t size;
      uint16_t kind;
      uint32_t element_type;
      uint32_t index_type;
      uint16_t length_in_bytes;
      char name[];
    } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_array.element_type);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_array.index_type);
  putc ('\n', asm_out_file);

  leaf_len = 13 + write_cv_integer (&t->lf_array.length_in_bytes);

  ASM_OUTPUT_ASCII (asm_out_file, "", 1);

  write_cv_padding (4 - (leaf_len % 4));

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write an LF_BITFIELD type.  */

static void
write_lf_bitfield (codeview_custom_type *t)
{
  /* This is lf_bitfield in binutils and lfBitfield in Microsoft's cvinfo.h:

    struct lf_bitfield
    {
      uint16_t size;
      uint16_t kind;
      uint32_t base_type;
      uint8_t length;
      uint8_t position;
    } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_bitfield.base_type);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (1, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_bitfield.length);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (1, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_bitfield.position);
  putc ('\n', asm_out_file);

  write_cv_padding (2);

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write an LF_PROCEDURE type.  Function pointers are implemented as pointers
   to one of these.  */

static void
write_lf_procedure (codeview_custom_type *t)
{
  /* This is lf_procedure in binutils and lfProc in Microsoft's cvinfo.h:

    struct lf_procedure
    {
      uint16_t size;
      uint16_t kind;
      uint32_t return_type;
      uint8_t calling_convention;
      uint8_t attributes;
      uint16_t num_parameters;
      uint32_t arglist;
    } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_procedure.return_type);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (1, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_procedure.calling_convention);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (1, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_procedure.attributes);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_procedure.num_parameters);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_procedure.arglist);
  putc ('\n', asm_out_file);

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write an LF_ARGLIST type.  This is just a list of other types.  LF_PROCEDURE
   entries point to one of these.  */

static void
write_lf_arglist (codeview_custom_type *t)
{
  /* This is lf_arglist in binutils and lfArgList in Microsoft's cvinfo.h:

    struct lf_arglist
    {
      uint16_t size;
      uint16_t kind;
      uint32_t num_entries;
      uint32_t args[];
    } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_arglist.num_entries);
  putc ('\n', asm_out_file);

  for (uint32_t i = 0; i < t->lf_arglist.num_entries; i++)
    {
      fputs (integer_asm_op (4, false), asm_out_file);
      fprint_whex (asm_out_file, t->lf_arglist.args[i]);
      putc ('\n', asm_out_file);
    }

  free (t->lf_arglist.args);

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write an LF_FUNC_ID type, which marries together a function type with its
   name.  This will end up in the alternative types stream in the final PDB,
   but we can just stick it in the normal .debug$T section.  */

static void
write_lf_func_id (codeview_custom_type *t)
{
  size_t name_len;

  /* This is lf_func_id in binutils and lfFuncId in Microsoft's cvinfo.h:

    struct lf_func_id
    {
      uint16_t size;
      uint16_t kind;
      uint32_t parent_scope;
      uint32_t function_type;
      char name[];
    } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_func_id.parent_scope);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_func_id.function_type);
  putc ('\n', asm_out_file);

  name_len = strlen (t->lf_func_id.name) + 1;

  ASM_OUTPUT_ASCII (asm_out_file, t->lf_func_id.name, name_len);

  write_cv_padding (4 - (name_len % 4));

  free (t->lf_func_id.name);

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write an LF_MFUNC_ID type, which is the version of LF_FUNC_ID for struct
   functions.  Instead of an LF_STRING_ID for the parent scope, we write the
   type number of the parent struct.  */

static void
write_lf_mfunc_id (codeview_custom_type *t)
{
  size_t name_len;

  /* This is lf_mfunc_id in binutils and lfMFuncId in Microsoft's cvinfo.h:

    struct lf_mfunc_id
    {
      uint16_t size;
      uint16_t kind;
      uint32_t parent_type;
      uint32_t function_type;
      char name[];
    } ATTRIBUTE_PACKED
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_mfunc_id.parent_type);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_mfunc_id.function_type);
  putc ('\n', asm_out_file);

  name_len = strlen (t->lf_mfunc_id.name) + 1;

  ASM_OUTPUT_ASCII (asm_out_file, t->lf_mfunc_id.name, name_len);

  write_cv_padding (4 - (name_len % 4));

  free (t->lf_mfunc_id.name);

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write an LF_STRING_ID type, which provides a deduplicated string that other
   types can reference.  */

static void
write_lf_string_id (codeview_custom_type *t)
{
  size_t string_len;

  /* This is lf_string_id in binutils and lfStringId in Microsoft's cvinfo.h:

    struct lf_string_id
    {
      uint16_t size;
      uint16_t kind;
      uint32_t substring;
      char string[];
    } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_string_id.substring);
  putc ('\n', asm_out_file);

  string_len = strlen (t->lf_string_id.string) + 1;

  ASM_OUTPUT_ASCII (asm_out_file, t->lf_string_id.string, string_len);

  write_cv_padding (4 - (string_len % 4));

  free (t->lf_string_id.string);

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write an LF_MFUNCTION type, representing a member function.  This is the
   struct-scoped equivalent of the LF_PROCEDURE type.  */

static void
write_lf_mfunction (codeview_custom_type *t)
{
  /* This is lf_mfunction in binutils and lfMFunc in Microsoft's cvinfo.h:

    struct lf_mfunction
    {
      uint16_t size;
      uint16_t kind;
      uint32_t return_type;
      uint32_t containing_class_type;
      uint32_t this_type;
      uint8_t calling_convention;
      uint8_t attributes;
      uint16_t num_parameters;
      uint32_t arglist;
      int32_t this_adjustment;
    } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_mfunction.return_type);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_mfunction.containing_class_type);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_mfunction.this_type);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (1, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_mfunction.calling_convention);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (1, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_mfunction.attributes);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_mfunction.num_parameters);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_mfunction.arglist);
  putc ('\n', asm_out_file);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, t->lf_mfunction.this_adjustment);
  putc ('\n', asm_out_file);

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write an LF_METHODLIST type, which is an array of type numbers for
   LF_MFUNCTION types.  Overloaded functions are represented by a LF_METHOD
   subtype in the field list, which points to a LF_METHODLIST type for the
   function's various forms.  */

static void
write_lf_methodlist (codeview_custom_type *t)
{
  /* This is lf_methodlist in binutils and lMethodList in Microsoft's cvinfo.h:

    struct lf_methodlist_entry
    {
      uint16_t method_attribute;
      uint16_t padding;
      uint32_t method_type;
    } ATTRIBUTE_PACKED;

    struct lf_methodlist
    {
      uint16_t size;
      uint16_t kind;
      struct lf_methodlist_entry entries[];
    } ATTRIBUTE_PACKED;
  */

  fputs (integer_asm_op (2, false), asm_out_file);
  asm_fprintf (asm_out_file, "%LLcv_type%x_end - %LLcv_type%x_start\n",
	       t->num, t->num);

  asm_fprintf (asm_out_file, "%LLcv_type%x_start:\n", t->num);

  fputs (integer_asm_op (2, false), asm_out_file);
  fprint_whex (asm_out_file, t->kind);
  putc ('\n', asm_out_file);

  for (unsigned int i = 0; i < t->lf_methodlist.count; i++)
    {
      fputs (integer_asm_op (2, false), asm_out_file);
      fprint_whex (asm_out_file, t->lf_methodlist.entries[i].method_attribute);
      putc ('\n', asm_out_file);

      fputs (integer_asm_op (2, false), asm_out_file);
      fprint_whex (asm_out_file, 0);
      putc ('\n', asm_out_file);

      fputs (integer_asm_op (4, false), asm_out_file);
      fprint_whex (asm_out_file, t->lf_methodlist.entries[i].method_type);
      putc ('\n', asm_out_file);
    }

  free (t->lf_methodlist.entries);

  asm_fprintf (asm_out_file, "%LLcv_type%x_end:\n", t->num);
}

/* Write the .debug$T section, which contains all of our custom type
   definitions.  */

static void
write_custom_types (void)
{
  targetm.asm_out.named_section (".debug$T", SECTION_DEBUG, NULL);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, CV_SIGNATURE_C13);
  putc ('\n', asm_out_file);

  while (custom_types)
    {
      codeview_custom_type *n = custom_types->next;

      switch (custom_types->kind)
	{
	case LF_POINTER:
	  write_lf_pointer (custom_types);
	  break;

	case LF_MODIFIER:
	  write_lf_modifier (custom_types);
	  break;

	case LF_FIELDLIST:
	  write_lf_fieldlist (custom_types);
	  break;

	case LF_ENUM:
	  write_lf_enum (custom_types);
	  break;

	case LF_STRUCTURE:
	case LF_CLASS:
	  write_lf_structure (custom_types);
	  break;

	case LF_UNION:
	  write_lf_union (custom_types);
	  break;

	case LF_ARRAY:
	  write_lf_array (custom_types);
	  break;

	case LF_BITFIELD:
	  write_lf_bitfield (custom_types);
	  break;

	case LF_PROCEDURE:
	  write_lf_procedure (custom_types);
	  break;

	case LF_ARGLIST:
	  write_lf_arglist (custom_types);
	  break;

	case LF_FUNC_ID:
	  write_lf_func_id (custom_types);
	  break;

	case LF_MFUNC_ID:
	  write_lf_mfunc_id (custom_types);
	  break;

	case LF_STRING_ID:
	  write_lf_string_id (custom_types);
	  break;

	case LF_MFUNCTION:
	  write_lf_mfunction (custom_types);
	  break;

	case LF_METHODLIST:
	  write_lf_methodlist (custom_types);
	  break;

	default:
	  break;
	}

      free (custom_types);
      custom_types = n;
    }
}

/* Finish CodeView debug info emission.  */

void
codeview_debug_finish (void)
{
  targetm.asm_out.named_section (".debug$S", SECTION_DEBUG, NULL);

  fputs (integer_asm_op (4, false), asm_out_file);
  fprint_whex (asm_out_file, CV_SIGNATURE_C13);
  putc ('\n', asm_out_file);

  write_strings_table ();
  write_source_files ();
  write_line_numbers ();

  if (inlinee_lines_htab)
    write_inlinee_lines ();

  write_codeview_symbols ();

  /* If we reference a nested struct but not its parent, add_deferred_type
     gets called if we create a forward reference for this, even though we've
     already flushed this in codeview_debug_early_finish.  In this case we will
     need to flush this list again.  */
  flush_deferred_types ();

  if (custom_types)
    write_custom_types ();

  while (funcs)
    {
      codeview_function *next_func = funcs->next;

      while (funcs->blocks)
	{
	  codeview_line_block *next_block = funcs->blocks->next;

	  while (funcs->blocks->lines)
	    {
	      codeview_line *next_line = funcs->blocks->lines->next;

	      free (funcs->blocks->lines);
	      funcs->blocks->lines = next_line;
	    }

	  free (funcs->blocks);
	  funcs->blocks = next_block;
	}

      free (funcs);
      funcs = next_func;
    }

  if (types_htab)
    delete types_htab;

  if (func_htab)
    delete func_htab;

  if (string_id_htab)
    delete string_id_htab;

  if (inlinee_lines_htab)
    delete inlinee_lines_htab;

  if (cv_func_htab)
    delete cv_func_htab;
}

/* Translate a DWARF base type (DW_TAG_base_type) into its CodeView
   equivalent.  */

static uint32_t
get_type_num_base_type (dw_die_ref type)
{
  unsigned int size = get_AT_unsigned (type, DW_AT_byte_size);

  switch (get_AT_unsigned (type, DW_AT_encoding))
    {
    case DW_ATE_signed_char:
      {
	const char *name = get_AT_string (type, DW_AT_name);

	if (size != 1)
	  return 0;

	if (name && !strcmp (name, "signed char"))
	  return T_CHAR;
	else
	  return T_RCHAR;
      }

    case DW_ATE_unsigned_char:
      if (size != 1)
	return 0;

      return T_UCHAR;

    case DW_ATE_signed:
      switch (size)
	{
	case 2:
	  return T_SHORT;

	case 4:
	  {
	    const char *name = get_AT_string (type, DW_AT_name);

	    if (name && !strcmp (name, "int"))
	      return T_INT4;
	    else
	      return T_LONG;
	  }

	case 8:
	  return T_QUAD;

	default:
	  return 0;
	}

    case DW_ATE_unsigned:
      switch (size)
	{
	case 2:
	  {
	    const char *name = get_AT_string (type, DW_AT_name);

	    if (name && !strcmp (name, "wchar_t"))
	      return T_WCHAR;
	    else
	      return T_USHORT;
	  }

	case 4:
	  {
	    const char *name = get_AT_string (type, DW_AT_name);

	    if (name && !strcmp (name, "unsigned int"))
	      return T_UINT4;
	    else
	      return T_ULONG;
	  }

	case 8:
	  return T_UQUAD;

	default:
	  return 0;
	}

    case DW_ATE_UTF:
      switch (size)
	{
	case 1:
	  return T_CHAR8;

	case 2:
	  return T_CHAR16;

	case 4:
	  return T_CHAR32;

	default:
	  return 0;
	}

    case DW_ATE_float:
      switch (size)
	{
	case 4:
	  return T_REAL32;

	case 8:
	  return T_REAL64;

	case 12:
	  return T_REAL80;

	case 16:
	  return T_REAL128;

	default:
	  return 0;
	}

    case DW_ATE_boolean:
      if (size == 1)
	return T_BOOL08;
      else
	return 0;

    default:
      return 0;
    }
}

/* Add a new codeview_custom_type to our singly-linked custom_types list.  */

static void
add_custom_type (codeview_custom_type *ct)
{
  uint32_t num;

  if (last_custom_type)
    {
      num = last_custom_type->num + 1;
      last_custom_type->next = ct;
    }
  else
    {
      num = FIRST_TYPE;
      custom_types = ct;
    }

  last_custom_type = ct;

  ct->num = num;
}

/* Process a DW_TAG_pointer_type DIE.  If this is a pointer to a builtin
   type, return the predefined constant for this.  Otherwise, add a new
   LF_POINTER type and return its number.  */

static uint32_t
get_type_num_pointer_type (dw_die_ref type, bool in_struct)
{
  uint32_t base_type_num, byte_size;
  dw_die_ref base_type;
  codeview_custom_type *ct;

  byte_size = get_AT_unsigned (type, DW_AT_byte_size);
  if (byte_size != 4 && byte_size != 8)
    return 0;

  base_type = get_AT_ref (type, DW_AT_type);

  /* If DW_AT_type is not set, this must be a void pointer.  */
  if (!base_type)
    return byte_size == 4 ? T_32PVOID : T_64PVOID;

  base_type_num = get_type_num (base_type, in_struct, false);
  if (base_type_num == 0)
    return 0;

  /* Pointers to builtin types have predefined type numbers, with the top byte
     determining the pointer size - 0x0400 for a 32-bit pointer and 0x0600
     for 64-bit.  */
  if (base_type_num < FIRST_TYPE && !(base_type_num & 0xff00))
    {
      if (byte_size == 4)
	return CV_POINTER_32 | base_type_num;
      else
	return CV_POINTER_64 | base_type_num;
    }

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_POINTER;
  ct->lf_pointer.base_type = base_type_num;

  if (byte_size == 4)
    ct->lf_pointer.attributes = CV_PTR_NEAR32;
  else
    ct->lf_pointer.attributes = CV_PTR_64;

  ct->lf_pointer.attributes |= byte_size << 13;

  add_custom_type (ct);

  return ct->num;
}

/* Process a DW_TAG_reference_type or DW_TAG_rvalue_reference_type DIE, add a
   new LF_POINTER type, and return its number.  */

static uint32_t
get_type_num_reference_type (dw_die_ref type, bool in_struct, bool rvref)
{
  uint32_t base_type_num, byte_size;
  dw_die_ref base_type;
  codeview_custom_type *ct;

  byte_size = get_AT_unsigned (type, DW_AT_byte_size);
  if (byte_size != 4 && byte_size != 8)
    return 0;

  base_type = get_AT_ref (type, DW_AT_type);

  base_type_num = get_type_num (base_type, in_struct, false);
  if (base_type_num == 0)
    return 0;

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_POINTER;
  ct->lf_pointer.base_type = base_type_num;
  ct->lf_pointer.attributes = rvref ? CV_PTR_MODE_RVREF : CV_PTR_MODE_LVREF;

  if (byte_size == 4)
    ct->lf_pointer.attributes |= CV_PTR_NEAR32;
  else
    ct->lf_pointer.attributes |= CV_PTR_64;

  ct->lf_pointer.attributes |= byte_size << 13;

  add_custom_type (ct);

  return ct->num;
}

/* Process a DW_TAG_const_type DIE, adding an LF_MODIFIER type and returning
   its number.  */

static uint32_t
get_type_num_const_type (dw_die_ref type, bool in_struct)
{
  dw_die_ref base_type;
  uint32_t base_type_num;
  codeview_custom_type *ct;
  bool is_volatile = false;

  base_type = get_AT_ref (type, DW_AT_type);

  /* Handle case when this is a const volatile type - we only need one
     LF_MODIFIER for this.  */
  if (base_type && dw_get_die_tag (base_type) == DW_TAG_volatile_type)
    {
      is_volatile = true;

      base_type = get_AT_ref (base_type, DW_AT_type);
    }

  if (!base_type)
    {
      base_type_num = T_VOID;
    }
  else
    {
      base_type_num = get_type_num (base_type, in_struct, false);
      if (base_type_num == 0)
	return 0;
    }

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_MODIFIER;
  ct->lf_modifier.base_type = base_type_num;
  ct->lf_modifier.modifier = MOD_const;

  if (is_volatile)
    ct->lf_modifier.modifier |= MOD_volatile;

  add_custom_type (ct);

  return ct->num;
}

/* Process a DW_TAG_volatile_type DIE, adding an LF_MODIFIER type and
   returning its number.  */

static uint32_t
get_type_num_volatile_type (dw_die_ref type, bool in_struct)
{
  dw_die_ref base_type;
  uint32_t base_type_num;
  codeview_custom_type *ct;

  base_type = get_AT_ref (type, DW_AT_type);

  if (base_type)
    {
      base_type_num = get_type_num (base_type, in_struct, false);
      if (base_type_num == 0)
	return 0;
    }
  else
    {
      base_type_num = T_VOID;
    }

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_MODIFIER;
  ct->lf_modifier.base_type = base_type_num;
  ct->lf_modifier.modifier = MOD_volatile;

  add_custom_type (ct);

  return ct->num;
}

/* Return the name of a DIE, traversing its parents in order to construct a
   C++-style name if necessary.  */
static char *
get_name (dw_die_ref die)
{
  dw_die_ref decl = get_AT_ref (die, DW_AT_specification);
  dw_die_ref parent;
  const char *name;
  char *str;
  size_t len;

  static const char anon[] = "<unnamed-tag>";
  static const char sep[] = "::";

  if (decl)
    die = decl;

  name = get_AT_string (die, DW_AT_name);

  if (!name)
    return NULL;

  parent = dw_get_die_parent (die);

  if (!parent || dw_get_die_tag (parent) == DW_TAG_compile_unit)
    return xstrdup (name);

  len = strlen (name);
  while (parent && dw_get_die_tag (parent) != DW_TAG_compile_unit)
    {
      const char *ns_name = get_AT_string (parent, DW_AT_name);

      len += sizeof (sep) - 1;

      if (ns_name)
	len += strlen (ns_name);
      else
	len += sizeof (anon) - 1;

      parent = dw_get_die_parent (parent);
    }

  str = (char *) xmalloc (len + 1);
  str[len] = 0;

  len -= strlen (name);
  memcpy (str + len, name, strlen (name));

  parent = dw_get_die_parent (die);
  while (parent && dw_get_die_tag (parent) != DW_TAG_compile_unit)
    {
      const char *ns_name = get_AT_string (parent, DW_AT_name);

      len -= sizeof (sep) - 1;
      memcpy (str + len, sep, sizeof (sep) - 1);

      if (ns_name)
	{
	  len -= strlen (ns_name);
	  memcpy (str + len, ns_name, strlen (ns_name));
	}
      else
	{
	  len -= sizeof (anon) - 1;
	  memcpy (str + len, anon, sizeof (anon) - 1);
	}

      parent = dw_get_die_parent (parent);
    }

  return str;
}

/* Add a forward declaration for an enum.  This is legal from C++11 onwards.  */

static uint32_t
add_enum_forward_def (dw_die_ref type)
{
  codeview_custom_type *ct;

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_ENUM;

  ct->lf_enum.count = 0;
  ct->lf_enum.properties = CV_PROP_FWDREF;
  ct->lf_enum.underlying_type = get_type_num (get_AT_ref (type, DW_AT_type),
					      false, false);
  ct->lf_enum.fieldlist = 0;
  ct->lf_enum.name = get_name (type);

  add_custom_type (ct);

  return ct->num;
}

/* Process a DW_TAG_enumeration_type DIE, adding an LF_FIELDLIST and an LF_ENUM
   type, returning the number of the latter.  */

static uint32_t
get_type_num_enumeration_type (dw_die_ref type, bool in_struct)
{
  dw_die_ref first_child;
  codeview_custom_type *ct;
  uint16_t count = 0;
  uint32_t last_type = 0;

  if (get_AT_flag (type, DW_AT_declaration))
    return add_enum_forward_def (type);

  /* First, add an LF_FIELDLIST for the enum's values.  We don't need to worry
     about deduplication here, as ld will take care of that for us.  If there's
     a lot of entries, add more LF_FIELDLISTs with LF_INDEXes pointing to
     the overflow lists.  */

  first_child = dw_get_die_child (type);

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_FIELDLIST;
  ct->lf_fieldlist.length = 0;
  ct->lf_fieldlist.subtypes = NULL;
  ct->lf_fieldlist.last_subtype = NULL;

  if (first_child)
    {
      dw_die_ref c;

      c = first_child;
      do
	{
	  dw_attr_node *att;
	  codeview_subtype *el;
	  size_t el_len;

	  c = dw_get_die_sib (c);

	  if (dw_get_die_tag (c) != DW_TAG_enumerator)
	    continue;

	  att = get_AT (c, DW_AT_const_value);
	  if (!att)
	    continue;

	  el = (codeview_subtype *) xmalloc (sizeof (*el));
	  el->next = NULL;
	  el->kind = LF_ENUMERATE;

	  switch (AT_class (att))
	    {
	    case dw_val_class_unsigned_const:
	    case dw_val_class_unsigned_const_implicit:
	      el->lf_enumerate.value.neg = false;
	      el->lf_enumerate.value.num = att->dw_attr_val.v.val_unsigned;
	      break;

	    case dw_val_class_const:
	    case dw_val_class_const_implicit:
	      if (att->dw_attr_val.v.val_int < 0)
		{
		  el->lf_enumerate.value.neg = true;
		  el->lf_enumerate.value.num = -att->dw_attr_val.v.val_int;
		}
	      else
		{
		  el->lf_enumerate.value.neg = false;
		  el->lf_enumerate.value.num = att->dw_attr_val.v.val_int;
		}
	      break;

	    default:
	      free (el);
	      continue;
	    }

	  el->lf_enumerate.name = xstrdup (get_AT_string (c, DW_AT_name));

	  el_len = 7 + strlen (el->lf_enumerate.name);
	  el_len += cv_integer_len (&el->lf_enumerate.value);

	  if (el_len % 4)
	    el_len += 4 - (el_len % 4);

	  if (ct->lf_fieldlist.length + el_len > MAX_FIELDLIST_SIZE)
	    {
	      codeview_subtype *idx;
	      codeview_custom_type *ct2;

	      idx = (codeview_subtype *) xmalloc (sizeof (*idx));
	      idx->next = NULL;
	      idx->kind = LF_INDEX;
	      idx->lf_index.type_num = 0;

	      ct->lf_fieldlist.last_subtype->next = idx;
	      ct->lf_fieldlist.last_subtype = idx;

	      ct2 = (codeview_custom_type *)
		xmalloc (sizeof (codeview_custom_type));

	      ct2->next = ct;
	      ct2->kind = LF_FIELDLIST;
	      ct2->lf_fieldlist.length = 0;
	      ct2->lf_fieldlist.subtypes = NULL;
	      ct2->lf_fieldlist.last_subtype = NULL;

	      ct = ct2;
	    }

	  ct->lf_fieldlist.length += el_len;

	  if (ct->lf_fieldlist.last_subtype)
	    ct->lf_fieldlist.last_subtype->next = el;
	  else
	    ct->lf_fieldlist.subtypes = el;

	  ct->lf_fieldlist.last_subtype = el;
	  count++;
	}
      while (c != first_child);
    }

  while (ct)
    {
      codeview_custom_type *ct2;

      ct2 = ct->next;
      ct->next = NULL;

      if (ct->lf_fieldlist.last_subtype->kind == LF_INDEX)
	ct->lf_fieldlist.last_subtype->lf_index.type_num = last_type;

      add_custom_type (ct);
      last_type = ct->num;

      ct = ct2;
    }

  /* Now add an LF_ENUM, pointing to the LF_FIELDLIST we just added.  */

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_ENUM;
  ct->lf_enum.count = count;
  ct->lf_enum.properties = 0;
  ct->lf_enum.underlying_type = get_type_num (get_AT_ref (type, DW_AT_type),
					      in_struct, false);
  ct->lf_enum.fieldlist = last_type;
  ct->lf_enum.name = get_name (type);

  add_custom_type (ct);

  return ct->num;
}

/* Add a DIE to our deferred_types list.  This happens when we have a struct
   with a pointer to a type that hasn't been defined yet, but which gets
   defined later on.  */

static void
add_deferred_type (dw_die_ref type)
{
  codeview_deferred_type *def;

  def = (codeview_deferred_type *) xmalloc (sizeof (codeview_deferred_type));

  def->next = NULL;
  def->type = type;

  if (!deferred_types)
    deferred_types = def;
  else
    last_deferred_type->next = def;

  last_deferred_type = def;
}

/* Flush the contents of our deferred_types list.  This happens after everything
   else has been written.  We call get_type_num to ensure that a type gets
   added to custom_types, if it hasn't been already.  */

static void
flush_deferred_types (void)
{
  while (deferred_types)
    {
      codeview_deferred_type *next;

      next = deferred_types->next;

      get_type_num (deferred_types->type, false, true);

      free (deferred_types);
      deferred_types = next;
    }

  last_deferred_type = NULL;
}

/* Add a forward definition for a struct, class, or union.  */

static uint32_t
add_struct_forward_def (dw_die_ref type)
{
  codeview_custom_type *ct;

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;

  switch (dw_get_die_tag (type))
    {
    case DW_TAG_class_type:
      ct->kind = LF_CLASS;
      break;

    case DW_TAG_structure_type:
      ct->kind = LF_STRUCTURE;
      break;

    case DW_TAG_union_type:
      ct->kind = LF_UNION;
      break;

    default:
      break;
    }

  ct->lf_structure.num_members = 0;
  ct->lf_structure.properties = CV_PROP_FWDREF;
  ct->lf_structure.field_list = 0;
  ct->lf_structure.derived_from = 0;
  ct->lf_structure.vshape = 0;
  ct->lf_structure.length.neg = false;
  ct->lf_structure.length.num = 0;
  ct->lf_structure.name = get_name (type);

  add_custom_type (ct);

  if (!get_AT_flag (type, DW_AT_declaration))
    add_deferred_type (type);

  return ct->num;
}

/* Add a new subtype to an LF_FIELDLIST type, and handle overflows if
   necessary.  */

static void
add_to_fieldlist (codeview_custom_type **ct, uint16_t *num_members,
		  codeview_subtype *el, size_t el_len)
{
  /* Add an LF_INDEX subtype if everything's too big for one
     LF_FIELDLIST.  */

  if ((*ct)->lf_fieldlist.length + el_len > MAX_FIELDLIST_SIZE)
    {
      codeview_subtype *idx;
      codeview_custom_type *ct2;

      idx = (codeview_subtype *) xmalloc (sizeof (*idx));
      idx->next = NULL;
      idx->kind = LF_INDEX;
      idx->lf_index.type_num = 0;

      (*ct)->lf_fieldlist.last_subtype->next = idx;
      (*ct)->lf_fieldlist.last_subtype = idx;

      ct2 = (codeview_custom_type *)
	xmalloc (sizeof (codeview_custom_type));

      ct2->next = *ct;
      ct2->kind = LF_FIELDLIST;
      ct2->lf_fieldlist.length = 0;
      ct2->lf_fieldlist.subtypes = NULL;
      ct2->lf_fieldlist.last_subtype = NULL;

      *ct = ct2;
    }

  (*ct)->lf_fieldlist.length += el_len;

  if ((*ct)->lf_fieldlist.last_subtype)
    (*ct)->lf_fieldlist.last_subtype->next = el;
  else
    (*ct)->lf_fieldlist.subtypes = el;

  (*ct)->lf_fieldlist.last_subtype = el;
  (*num_members)++;
}

/* Add an LF_BITFIELD type, returning its number.  DWARF represents bitfields
   as members in a struct with a DW_AT_data_bit_offset attribute, whereas in
   CodeView they're a distinct type.  */

static uint32_t
create_bitfield (dw_die_ref c)
{
  codeview_custom_type *ct;
  uint32_t base_type;

  base_type = get_type_num (get_AT_ref (c, DW_AT_type), true, false);
  if (base_type == 0)
    return 0;

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_BITFIELD;
  ct->lf_bitfield.base_type = base_type;
  ct->lf_bitfield.length = get_AT_unsigned (c, DW_AT_bit_size);
  ct->lf_bitfield.position = get_AT_unsigned (c, DW_AT_data_bit_offset);

  add_custom_type (ct);

  return ct->num;
}

/* Create an LF_MEMBER field list subtype for a struct member, returning its
   pointer in el and its size in el_len.  */

static void
add_struct_member (dw_die_ref c, uint16_t accessibility,
		   codeview_custom_type **ct, uint16_t *num_members,
		   unsigned int base_offset)
{
  codeview_subtype *el;
  size_t el_len;
  dw_die_ref type = get_AT_ref (c, DW_AT_type);
  unsigned int offset;

  offset = base_offset + get_AT_unsigned (c, DW_AT_data_member_location);

  /* If the data member is actually an anonymous struct, class, or union,
     follow MSVC by flattening this into its parent.  */
  if (!get_AT_string (c, DW_AT_name) && type
      && (dw_get_die_tag (type) == DW_TAG_structure_type
	  || dw_get_die_tag (type) == DW_TAG_class_type
	  || dw_get_die_tag (type) == DW_TAG_union_type))
    {
      dw_die_ref c2, first_child;

      first_child = dw_get_die_child (type);
      c2 = first_child;

      do
	{
	  c2 = dw_get_die_sib (c2);

	  if (dw_get_die_tag (c2) == DW_TAG_member)
	      add_struct_member (c2, accessibility, ct, num_members, offset);
	}
      while (c2 != first_child);

      return;
    }

  el = (codeview_subtype *) xmalloc (sizeof (*el));
  el->next = NULL;
  el->kind = LF_MEMBER;
  el->lf_member.attributes = accessibility;

  if (get_AT (c, DW_AT_data_bit_offset))
    el->lf_member.type = create_bitfield (c);
  else
    el->lf_member.type = get_type_num (type, true, false);

  el->lf_member.offset.neg = false;
  el->lf_member.offset.num = offset;

  el_len = 11 + cv_integer_len (&el->lf_member.offset);

  if (get_AT_string (c, DW_AT_name))
    {
      el->lf_member.name = xstrdup (get_AT_string (c, DW_AT_name));
      el_len += strlen (el->lf_member.name);
    }
  else
    {
      el->lf_member.name = NULL;
    }

  if (el_len % 4)
    el_len += 4 - (el_len % 4);

  add_to_fieldlist (ct, num_members, el, el_len);
}

/* Create an LF_STMEMBER field list subtype for a static struct member,
   returning its pointer in el and its size in el_len.  */

static void
add_struct_static_member (dw_die_ref c, uint16_t accessibility,
			  codeview_custom_type **ct, uint16_t *num_members)
{
  codeview_subtype *el;
  size_t el_len;

  el = (codeview_subtype *) xmalloc (sizeof (*el));
  el->next = NULL;
  el->kind = LF_STMEMBER;
  el->lf_static_member.attributes = accessibility;
  el->lf_static_member.type = get_type_num (get_AT_ref (c, DW_AT_type),
					    true, false);
  el->lf_static_member.name = xstrdup (get_AT_string (c, DW_AT_name));

  el_len = 9 + strlen (el->lf_static_member.name);

  if (el_len % 4)
    el_len += 4 - (el_len % 4);

  add_to_fieldlist (ct, num_members, el, el_len);
}

/* Create a field list subtype for a struct function, returning its pointer in
   el and its size in el_len.  If the function is not overloaded, create an
   LF_ONEMETHOD subtype pointing to the LF_MFUNCTION.  Otherwise, add an
   LF_METHODLIST type of the function's forms, and create an LF_METHOD subtype
   pointing to this.  */

static void
add_struct_function (dw_die_ref c, hash_table<method_hasher> *method_htab,
		     codeview_custom_type **ct, uint16_t *num_members)
{
  const char *name = get_AT_string (c, DW_AT_name);
  codeview_method **slot, *meth;
  codeview_subtype *el;
  size_t el_len;

  slot = method_htab->find_slot_with_hash (name, htab_hash_string (name),
					   NO_INSERT);
  if (!slot)
    return;

  meth = *slot;

  el = (codeview_subtype *) xmalloc (sizeof (*el));
  el->next = NULL;

  if (meth->count == 1)
    {
      el->kind = LF_ONEMETHOD;
      el->lf_onemethod.method_attribute = meth->attribute;
      el->lf_onemethod.method_type = meth->type;
      el->lf_onemethod.name = xstrdup (name);

      el_len = 9 + strlen (el->lf_onemethod.name);
    }
  else
    {
      codeview_custom_type *ct;
      lf_methodlist_entry *ent;

      ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

      ct->next = NULL;
      ct->kind = LF_METHODLIST;
      ct->lf_methodlist.count = meth->count;
      ct->lf_methodlist.entries = (lf_methodlist_entry *)
	xmalloc (meth->count * sizeof (lf_methodlist_entry));

      ent = ct->lf_methodlist.entries;
      for (codeview_method *m = meth; m; m = m->next)
	{
	  ent->method_attribute = m->attribute;
	  ent->method_type = m->type;
	  ent++;
	}

      add_custom_type (ct);

      el->kind = LF_METHOD;
      el->lf_method.count = meth->count;
      el->lf_method.method_list = ct->num;
      el->lf_method.name = xstrdup (name);

      el_len = 9 + strlen (el->lf_method.name);
    }

  if (el_len % 4)
    el_len += 4 - (el_len % 4);

  add_to_fieldlist (ct, num_members, el, el_len);

  method_htab->remove_elt_with_hash (name, htab_hash_string (name));

  while (meth)
    {
      codeview_method *next = meth->next;

      free (meth->name);
      free (meth);
      meth = next;
    }
}

/* Create a field list subtype that records the base class that a struct
   inherits from.  */

static void
add_struct_inheritance (dw_die_ref c, uint16_t accessibility,
			codeview_custom_type **ct, uint16_t *num_members)
{
  codeview_subtype *el;
  size_t el_len;

  /* FIXME: if DW_AT_virtuality is DW_VIRTUALITY_virtual this is a virtual
	    base class, and we should be issuing an LF_VBCLASS record
	    instead.  */
  if (get_AT_unsigned (c, DW_AT_virtuality) == DW_VIRTUALITY_virtual)
    return;

  el = (codeview_subtype *) xmalloc (sizeof (*el));
  el->next = NULL;
  el->kind = LF_BCLASS;
  el->lf_bclass.attributes = accessibility;
  el->lf_bclass.base_class_type = get_type_num (get_AT_ref (c, DW_AT_type),
						   true, false);
  el->lf_bclass.offset.neg = false;
  el->lf_bclass.offset.num = get_AT_unsigned (c, DW_AT_data_member_location);

  el_len = 10 + cv_integer_len (&el->lf_bclass.offset);

  if (el_len % 4)
    el_len += 4 - (el_len % 4);

  add_to_fieldlist (ct, num_members, el, el_len);
}

/* Create a new LF_MFUNCTION type for a struct function, add it to the
   types_htab hash table, and return its type number.  */

static uint32_t
get_mfunction_type (dw_die_ref c)
{
  uint32_t containing_class_type, this_type, mfunction_type;
  dw_die_ref obj_pointer;
  codeview_type **slot, *t;

  containing_class_type = get_type_num (dw_get_die_parent (c), true, false);

  obj_pointer = get_AT_ref (c, DW_AT_object_pointer);
  if (obj_pointer && dw_get_die_tag (obj_pointer) == DW_TAG_formal_parameter)
    {
      this_type = get_type_num (get_AT_ref (obj_pointer, DW_AT_type),
				true, false);
    }
  else
    {
      this_type = 0;
    }

  mfunction_type = get_type_num_subroutine_type (c, true, containing_class_type,
						 this_type, 0);

  slot = types_htab->find_slot_with_hash (c, htab_hash_pointer (c), INSERT);

  t = (codeview_type *) xmalloc (sizeof (codeview_type));

  t->die = c;
  t->num = mfunction_type;
  t->is_fwd_ref = false;

  *slot = t;

  return mfunction_type;
}

/* Translate a DWARF DW_AT_accessibility constant into its CodeView
   equivalent.  If implicit, follow the C++ rules.  */

static uint16_t
get_accessibility (dw_die_ref c)
{
  switch (get_AT_unsigned (c, DW_AT_accessibility))
    {
    case DW_ACCESS_private:
      return CV_ACCESS_PRIVATE;

    case DW_ACCESS_protected:
      return CV_ACCESS_PROTECTED;

    case DW_ACCESS_public:
      return CV_ACCESS_PUBLIC;

    /* Members in a C++ struct or union are public by default, members
      in a class are private.  */
    default:
      if (dw_get_die_tag (dw_get_die_parent (c)) == DW_TAG_class_type)
	return CV_ACCESS_PRIVATE;
      else
	return CV_ACCESS_PUBLIC;
    }
}

/* Returns true if the struct function pointed to by die is an instantiated
   template function.  These are skipped in CodeView struct definitions, as
   otherwise the same type might not be deduplicated across different TUs.  */

static bool
is_templated_func (dw_die_ref die)
{
  dw_die_ref c = dw_get_die_child (die);

  if (!c)
    return false;

  do
    {
      c = dw_get_die_sib (c);

      if (dw_get_die_tag (c) == DW_TAG_template_type_param)
	return true;
    }
  while (c != dw_get_die_child (die));

  return false;
}

/* Create a field list subtype that records that a struct has a nested type
   contained within it.  */

static void
add_struct_nested_type (dw_die_ref c, codeview_custom_type **ct,
			uint16_t *num_members)
{
  const char *name = get_AT_string (c, DW_AT_name);
  codeview_subtype *el;
  size_t name_len, el_len;

  if (!name)
    return;

  name_len = strlen (name);

  el = (codeview_subtype *) xmalloc (sizeof (*el));
  el->next = NULL;
  el->kind = LF_NESTTYPE;
  el->lf_nesttype.type = get_type_num (c, true, false);
  el->lf_nesttype.name = xstrdup (name);

  el_len = 9 + name_len;

  if (el_len % 4)
    el_len += 4 - (el_len % 4);

  add_to_fieldlist (ct, num_members, el, el_len);
}

/* Process a DW_TAG_structure_type, DW_TAG_class_type, or DW_TAG_union_type
   DIE, add an LF_FIELDLIST and an LF_STRUCTURE / LF_CLASS / LF_UNION type,
   and return the number of the latter.  */

static uint32_t
get_type_num_struct (dw_die_ref type, bool in_struct, bool *is_fwd_ref)
{
  dw_die_ref parent, first_child;
  codeview_custom_type *ct;
  uint16_t num_members = 0;
  uint32_t last_type = 0;

  parent = dw_get_die_parent(type);

  if (parent && (dw_get_die_tag (parent) == DW_TAG_structure_type
      || dw_get_die_tag (parent) == DW_TAG_class_type
      || dw_get_die_tag (parent) == DW_TAG_union_type))
    get_type_num (parent, true, false);

  if ((in_struct && get_AT_string (type, DW_AT_name))
      || get_AT_flag (type, DW_AT_declaration))
    {
      *is_fwd_ref = true;
      return add_struct_forward_def (type);
    }

  *is_fwd_ref = false;

  /* First, add an LF_FIELDLIST for the structure's members.  We don't need to
     worry about deduplication here, as ld will take care of that for us.
     If there's a lot of entries, add more LF_FIELDLISTs with LF_INDEXes
     pointing to the overflow lists.  */

  first_child = dw_get_die_child (type);

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_FIELDLIST;
  ct->lf_fieldlist.length = 0;
  ct->lf_fieldlist.subtypes = NULL;
  ct->lf_fieldlist.last_subtype = NULL;

  if (first_child)
    {
      hash_table<method_hasher> *method_htab = NULL;
      dw_die_ref c;

      /* First, loop through and record any non-templated member functions.
	 This is because overloaded and non-overloaded functions are expressed
	 differently in CodeView, so we need to have a hash table on the name
	 to know how to record it later on.  */

      c = first_child;
      do
	{
	  c = dw_get_die_sib (c);

	  if (dw_get_die_tag (c) == DW_TAG_subprogram)
	    {
	      const char *name = get_AT_string (c, DW_AT_name);
	      codeview_method *meth, **slot;

	      if (is_templated_func (c))
		continue;

	      if (!method_htab)
		method_htab = new hash_table<method_hasher> (10);

	      meth = (codeview_method *) xmalloc (sizeof (*meth));

	      slot = method_htab->find_slot_with_hash (name,
						       htab_hash_string (name),
						       INSERT);

	      meth->attribute = get_accessibility (c);

	      if (!get_AT_ref (c, DW_AT_object_pointer))
		meth->attribute |= CV_METHOD_STATIC;

	      meth->type = get_mfunction_type (c);
	      meth->next = NULL;

	      if (*slot)
		{
		  if ((*slot)->last)
		    (*slot)->last->next = meth;
		  else
		    (*slot)->next = meth;

		  (*slot)->last = meth;
		  (*slot)->count++;

		  meth->name = NULL;
		}
	      else
		{
		  meth->name = xstrdup (name);
		  meth->last = NULL;
		  meth->count = 1;
		  *slot = meth;
		}
	    }
	}
      while (c != first_child);

      /* Now loop through again and record the actual members.  */

      c = first_child;
      do
	{
	  uint16_t accessibility;

	  c = dw_get_die_sib (c);

	  accessibility = get_accessibility (c);

	  switch (dw_get_die_tag (c))
	    {
	    case DW_TAG_member:
	      add_struct_member (c, accessibility, &ct, &num_members, 0);
	      break;

	    case DW_TAG_variable:
	      add_struct_static_member (c, accessibility, &ct, &num_members);
	      break;

	    case DW_TAG_subprogram:
	      if (!is_templated_func (c))
		add_struct_function (c, method_htab, &ct, &num_members);
	      break;

	    case DW_TAG_inheritance:
	      add_struct_inheritance (c, accessibility, &ct, &num_members);
	      break;

	    case DW_TAG_structure_type:
	    case DW_TAG_class_type:
	    case DW_TAG_union_type:
	    case DW_TAG_enumeration_type:
	      add_struct_nested_type (c, &ct, &num_members);
	      break;

	    default:
	      break;
	    }
	}
      while (c != first_child);

      if (method_htab)
	delete method_htab;
    }

  while (ct)
    {
      codeview_custom_type *ct2;

      ct2 = ct->next;
      ct->next = NULL;

      if (ct->lf_fieldlist.last_subtype
	  && ct->lf_fieldlist.last_subtype->kind == LF_INDEX)
	{
	  ct->lf_fieldlist.last_subtype->lf_index.type_num = last_type;
	}

      add_custom_type (ct);
      last_type = ct->num;

      ct = ct2;
    }

  /* Now add an LF_STRUCTURE / LF_CLASS / LF_UNION, pointing to the
     LF_FIELDLIST we just added.  */

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;

  switch (dw_get_die_tag (type))
    {
    case DW_TAG_class_type:
      ct->kind = LF_CLASS;
      break;

    case DW_TAG_structure_type:
      ct->kind = LF_STRUCTURE;
      break;

    case DW_TAG_union_type:
      ct->kind = LF_UNION;
      break;

    default:
      break;
    }

  ct->lf_structure.num_members = num_members;
  ct->lf_structure.properties = 0;
  ct->lf_structure.field_list = last_type;
  ct->lf_structure.derived_from = 0;
  ct->lf_structure.vshape = 0;
  ct->lf_structure.length.neg = false;
  ct->lf_structure.length.num = get_AT_unsigned (type, DW_AT_byte_size);
  ct->lf_structure.name = get_name (type);

  add_custom_type (ct);

  return ct->num;
}

/* Process a DW_TAG_subroutine_type DIE, adding an LF_ARGLIST and an
   LF_PROCEDURE or LF_MFUNCTION type, and returning the number of the
   latter.  */

static uint32_t
get_type_num_subroutine_type (dw_die_ref type, bool in_struct,
			      uint32_t containing_class_type,
			      uint32_t this_type, int32_t this_adjustment)
{
  codeview_custom_type *ct;
  uint32_t return_type, arglist_type;
  uint16_t num_args;
  dw_die_ref first_child;

  /* Find the return type.  */

  if (get_AT_ref (type, DW_AT_type))
    {
      return_type = get_type_num (get_AT_ref (type, DW_AT_type), in_struct,
				  false);
      if (return_type == 0)
	return 0;
    }
  else
    {
      return_type = T_VOID;
    }

  /* Handle pointer to member function.  */
  if (containing_class_type == 0)
    {
      dw_die_ref obj_ptr = get_AT_ref (type, DW_AT_object_pointer);

      if (obj_ptr)
	{
	  dw_die_ref obj_ptr_type = get_AT_ref (obj_ptr, DW_AT_type);

	  if (obj_ptr_type
	      && dw_get_die_tag (obj_ptr_type) == DW_TAG_pointer_type)
	    {
	      dw_die_ref cont_class = get_AT_ref (obj_ptr_type, DW_AT_type);

	      if (dw_get_die_tag (cont_class) == DW_TAG_const_type)
		cont_class = get_AT_ref (cont_class, DW_AT_type);

	      containing_class_type = get_type_num (cont_class, in_struct,
						    false);
	      this_type = get_type_num (obj_ptr_type, in_struct, false);
	    }
	}
    }

  /* Count the arguments.  */

  first_child = dw_get_die_child (type);
  num_args = 0;

  if (first_child)
    {
      dw_die_ref c;

      c = first_child;
      do
	{
	  c = dw_get_die_sib (c);

	  if (dw_get_die_tag (c) != DW_TAG_formal_parameter
	      && dw_get_die_tag (c) != DW_TAG_unspecified_parameters)
	    continue;

	  /* We ignore "this" params here.  */
	  if (get_AT_flag (c, DW_AT_artificial) != 0)
	    continue;

	  num_args++;
	}
      while (c != first_child);
    }

  /* Create an LF_ARGLIST for the arguments.  If this is a duplicate, ld
     will take care of this for us.  */

  first_child = dw_get_die_child (type);

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_ARGLIST;
  ct->lf_arglist.num_entries = num_args;

  if (num_args > 0)
    {
      dw_die_ref c;
      uint32_t *argptr;

      ct->lf_arglist.args = (uint32_t *) xmalloc (sizeof (uint32_t) * num_args);
      argptr = ct->lf_arglist.args;

      c = first_child;
      do
	{
	  c = dw_get_die_sib (c);

	  if (get_AT_flag (c, DW_AT_artificial) != 0)
	    continue;

	  switch (dw_get_die_tag (c))
	    {
	    case DW_TAG_formal_parameter:
	      *argptr = get_type_num (get_AT_ref (c, DW_AT_type), in_struct,
				      false);
	      argptr++;
	      break;

	    case DW_TAG_unspecified_parameters:
	      *argptr = 0;
	      argptr++;
	      break;

	    default:
	      break;
	    }
	}
      while (c != first_child);
    }
  else
    {
      ct->lf_arglist.args = NULL;
    }

  add_custom_type (ct);

  arglist_type = ct->num;

  /* Finally, create an LF_PROCEDURE or LF_MFUNCTION type.  */

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;

  if (containing_class_type != 0)
    {
      ct->kind = LF_MFUNCTION;
      ct->lf_mfunction.return_type = return_type;
      ct->lf_mfunction.containing_class_type = containing_class_type;
      ct->lf_mfunction.this_type = this_type;
      ct->lf_mfunction.calling_convention = 0;
      ct->lf_mfunction.attributes = 0;
      ct->lf_mfunction.num_parameters = num_args;
      ct->lf_mfunction.arglist = arglist_type;
      ct->lf_mfunction.this_adjustment = this_adjustment;
    }
  else
    {
      ct->kind = LF_PROCEDURE;
      ct->lf_procedure.return_type = return_type;
      ct->lf_procedure.calling_convention = 0;
      ct->lf_procedure.attributes = 0;
      ct->lf_procedure.num_parameters = num_args;
      ct->lf_procedure.arglist = arglist_type;
    }

  add_custom_type (ct);

  return ct->num;
}

/* Process a DW_TAG_array_type DIE, adding an LF_ARRAY type and returning its
   number.  */

static uint32_t
get_type_num_array_type (dw_die_ref type, bool in_struct)
{
  dw_die_ref base_type, t, first_child, c, *dimension_arr;
  uint64_t size = 0;
  unsigned int dimensions, i;
  uint32_t element_type;

  base_type = get_AT_ref (type, DW_AT_type);
  if (!base_type)
    return 0;

  /* We need to know the size of our base type.  Loop through until we find
     it.  */
  t = base_type;
  while (t && size == 0)
    {
      switch (dw_get_die_tag (t))
	{
	case DW_TAG_const_type:
	case DW_TAG_volatile_type:
	case DW_TAG_typedef:
	case DW_TAG_enumeration_type:
	  t = get_AT_ref (t, DW_AT_type);
	  break;

	case DW_TAG_base_type:
	case DW_TAG_structure_type:
	case DW_TAG_class_type:
	case DW_TAG_union_type:
	case DW_TAG_pointer_type:
	case DW_TAG_reference_type:
	case DW_TAG_rvalue_reference_type:
	  size = get_AT_unsigned (t, DW_AT_byte_size);
	  break;

	default:
	  return 0;
	}
    }

  if (size == 0)
    return 0;

  first_child = dw_get_die_child (type);
  if (!first_child)
    return 0;

  element_type = get_type_num (base_type, in_struct, false);
  if (element_type == 0)
    return 0;

  /* Create an array of our DW_TAG_subrange_type children, in reverse order.
     We have to do this because unlike DWARF CodeView doesn't have
     multidimensional arrays, so instead we do arrays of arrays.  */

  dimensions = 0;
  c = first_child;
  do
    {
      c = dw_get_die_sib (c);
      if (dw_get_die_tag (c) != DW_TAG_subrange_type)
	continue;

      dimensions++;
    }
  while (c != first_child);

  if (dimensions == 0)
    return 0;

  dimension_arr = (dw_die_ref *) xmalloc (sizeof (dw_die_ref) * dimensions);

  c = first_child;
  i = 0;
  do
    {
      c = dw_get_die_sib (c);
      if (dw_get_die_tag (c) != DW_TAG_subrange_type)
	continue;

      dimension_arr[dimensions - i - 1] = c;
      i++;
    }
  while (c != first_child);

  /* Record an LF_ARRAY entry for each array dimension.  If this leads to
     duplicate types, ld will take care of it for us.  */

  for (i = 0; i < dimensions; i++)
    {
      codeview_custom_type *ct;
      dw_die_ref index;

      ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

      size *= get_AT_unsigned (dimension_arr[i], DW_AT_upper_bound) + 1;

      index = get_AT_ref (dimension_arr[i], DW_AT_type);

      ct->next = NULL;
      ct->kind = LF_ARRAY;
      ct->lf_array.element_type = element_type;
      ct->lf_array.index_type = get_type_num (index, in_struct, false);
      ct->lf_array.length_in_bytes.neg = false;
      ct->lf_array.length_in_bytes.num = size;

      add_custom_type (ct);

      element_type = ct->num;
    }

  free (dimension_arr);

  return element_type;
}

/* Translate a DW_TAG_ptr_to_member_type DIE, that is a pointer to member
   function or field, into an LF_POINTER record.  */

static uint32_t
get_type_num_ptr_to_member_type (dw_die_ref type, bool in_struct)
{
  uint32_t base_type_num;
  uint32_t containing_class;
  dw_die_ref base_type;
  codeview_custom_type *ct;

  base_type = get_AT_ref (type, DW_AT_type);

  base_type_num = get_type_num (base_type, in_struct, false);
  if (base_type_num == 0)
    return 0;

  containing_class = get_type_num (get_AT_ref (type, DW_AT_containing_type),
				   in_struct, false);

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_POINTER;
  ct->lf_pointer.base_type = base_type_num;

  if (TARGET_64BIT)
    {
      ct->lf_pointer.attributes = CV_PTR_64;
      ct->lf_pointer.attributes |= 8 << 13;
    }
  else
    {
      ct->lf_pointer.attributes = CV_PTR_NEAR32;
      ct->lf_pointer.attributes |= 4 << 13;
    }

  ct->lf_pointer.containing_class = containing_class;

  if (base_type && dw_get_die_tag (base_type) == DW_TAG_subroutine_type)
    {
      ct->lf_pointer.attributes |= CV_PTR_MODE_PMFUNC;
      ct->lf_pointer.ptr_to_mem_type = CV_PMTYPE_F_Single;
    }
  else
    {
      ct->lf_pointer.attributes |= CV_PTR_MODE_PMEM;
      ct->lf_pointer.ptr_to_mem_type = CV_PMTYPE_D_Single;
    }

  add_custom_type (ct);

  return ct->num;
}

/* Return the type number that corresponds to a DW_TAG_typedef DIE: either the
   type number of the base type, or follow MSVC in having a special value
   for the HRESULT used by COM.  */

static uint32_t
get_type_num_typedef (dw_die_ref type, bool in_struct)
{
  uint32_t num;

  num = get_type_num (get_AT_ref (type, DW_AT_type), in_struct, false);

  if (num == T_LONG)
    {
      const char *name = get_AT_string (type, DW_AT_name);

      /* longs typedef'd as "HRESULT" get their own type */
      if (name && !strcmp (name, "HRESULT"))
	num = T_HRESULT;
    }

  return num;
}

/* Process a DIE representing a type definition, add a CodeView type if
   necessary, and return its number.  If it's something we can't handle, return
   0.  We keep a hash table so that we're not adding the same type multiple
   times - though if we do it's not disastrous, as ld will deduplicate
   everything for us.  */

static uint32_t
get_type_num (dw_die_ref type, bool in_struct, bool no_fwd_ref)
{
  codeview_type **slot, *t;
  uint32_t num;
  bool is_fwd_ref;

  if (!type)
    return 0;

  if (!types_htab)
    types_htab = new hash_table<die_hasher> (10);

  slot = types_htab->find_slot_with_hash (type, htab_hash_pointer (type),
					  NO_INSERT);

  if (slot && *slot && (!no_fwd_ref || !(*slot)->is_fwd_ref))
    return (*slot)->num;

  is_fwd_ref = false;

  switch (dw_get_die_tag (type))
    {
    case DW_TAG_base_type:
      num = get_type_num_base_type (type);
      break;

    case DW_TAG_typedef:
      num = get_type_num_typedef (type, in_struct);
      break;

    case DW_TAG_pointer_type:
      num = get_type_num_pointer_type (type, in_struct);
      break;

    case DW_TAG_reference_type:
      num = get_type_num_reference_type (type, in_struct, false);
      break;

    case DW_TAG_rvalue_reference_type:
      num = get_type_num_reference_type (type, in_struct, true);
      break;

    case DW_TAG_const_type:
      num = get_type_num_const_type (type, in_struct);
      break;

    case DW_TAG_volatile_type:
      num = get_type_num_volatile_type (type, in_struct);
      break;

    case DW_TAG_enumeration_type:
      num = get_type_num_enumeration_type (type, in_struct);
      break;

    case DW_TAG_structure_type:
    case DW_TAG_class_type:
    case DW_TAG_union_type:
      num = get_type_num_struct (type, in_struct, &is_fwd_ref);
      break;

    case DW_TAG_array_type:
      num = get_type_num_array_type (type, in_struct);
      break;

    case DW_TAG_subroutine_type:
      num = get_type_num_subroutine_type (type, in_struct, 0, 0, 0);
      break;

    case DW_TAG_ptr_to_member_type:
      num = get_type_num_ptr_to_member_type (type, in_struct);
      break;

    default:
      num = 0;
      break;
    }

  /* Check hash table again, and account for the fact that self-referential
     structs will have created a forward reference to themselves.  */

  slot = types_htab->find_slot_with_hash (type, htab_hash_pointer (type),
					  INSERT);

  if (*slot && (*slot)->is_fwd_ref && !is_fwd_ref)
    {
      (*slot)->num = num;
      (*slot)->is_fwd_ref = false;
      return num;
    }

  t = (codeview_type *) xmalloc (sizeof (codeview_type));
  t->die = type;
  t->num = num;
  t->is_fwd_ref = is_fwd_ref;

  *slot = t;

  return t->num;
}

/* Process a DW_TAG_variable DIE, and add an S_GDATA32 or S_LDATA32 symbol for
   this.  */

static void
add_variable (dw_die_ref die)
{
  codeview_symbol *s;
  const char *name;

  name = get_AT_string (die, DW_AT_name);
  if (!name)
    return;

  s = (codeview_symbol *) xmalloc (sizeof (codeview_symbol));

  s->next = NULL;
  s->kind = get_AT (die, DW_AT_external) ? S_GDATA32 : S_LDATA32;
  s->data_symbol.type = get_type_num (get_AT_ref (die, DW_AT_type), false,
				      false);
  s->data_symbol.name = get_name (die);
  s->data_symbol.die = die;

  if (last_sym)
    last_sym->next = s;
  else
    sym = s;

  last_sym = s;
}

/* Return the type number of the LF_STRING_ID entry corresponding to the given
   string, creating a new one if necessary.  */

static uint32_t
add_string_id (const char *s)
{
  codeview_custom_type **slot;
  codeview_custom_type *ct;

  if (!string_id_htab)
    string_id_htab = new hash_table<string_id_hasher> (10);

  slot = string_id_htab->find_slot_with_hash (s, htab_hash_string (s),
					      INSERT);
  if (*slot)
    return (*slot)->num;

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_STRING_ID;
  ct->lf_string_id.substring = 0;
  ct->lf_string_id.string = xstrdup (s);

  add_custom_type (ct);

  *slot = ct;

  return ct->num;
}

/* Return the type number of the LF_STRING_ID corresponding to the given DIE's
   parent, or 0 if it is in the global scope.  */

static uint32_t
get_scope_string_id (dw_die_ref die)
{
  dw_die_ref decl = get_AT_ref (die, DW_AT_specification);
  char *name;
  uint32_t ret;

  if (decl)
    die = decl;

  die = dw_get_die_parent (die);
  if (!die)
    return 0;

  if (dw_get_die_tag (die) == DW_TAG_compile_unit)
    return 0;

  name = get_name (die);
  if (!name)
    return 0;

  ret = add_string_id (name);
  free (name);

  return ret;
}

/* Add an LF_FUNC_ID type and return its number (see write_lf_func_id).  */

static uint32_t
add_lf_func_id (dw_die_ref die, const char *name)
{
  uint32_t function_type, scope_type;
  codeview_custom_type *ct;

  function_type = get_type_num_subroutine_type (die, false, 0, 0, 0);
  scope_type = get_scope_string_id (die);

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_FUNC_ID;
  ct->lf_func_id.parent_scope = scope_type;
  ct->lf_func_id.function_type = function_type;
  ct->lf_func_id.name = xstrdup (name);

  add_custom_type (ct);

  return ct->num;
}

/* Add an LF_MFUNC_ID type and return its number (see write_lf_mfunc_id).  */

static uint32_t
add_lf_mfunc_id (dw_die_ref die, const char *name)
{
  uint32_t function_type = 0, parent_type;
  codeview_custom_type *ct;
  dw_die_ref spec = get_AT_ref (die, DW_AT_specification);

  parent_type = get_type_num (dw_get_die_parent (spec), false, false);

  if (types_htab)
    {
      codeview_type **slot;

      slot = types_htab->find_slot_with_hash (spec, htab_hash_pointer (spec),
					      NO_INSERT);

      if (slot && *slot)
	function_type = (*slot)->num;
    }

  if (function_type == 0)
    {
      function_type = get_type_num_subroutine_type (die, false, parent_type,
						    0, 0);
    }

  ct = (codeview_custom_type *) xmalloc (sizeof (codeview_custom_type));

  ct->next = NULL;
  ct->kind = LF_MFUNC_ID;
  ct->lf_mfunc_id.parent_type = parent_type;
  ct->lf_mfunc_id.function_type = function_type;
  ct->lf_mfunc_id.name = xstrdup (name);

  add_custom_type (ct);

  return ct->num;
}

/* Generate a new LF_FUNC_ID or LF_MFUNC_ID type for a DW_TAG_subprogram DIE
   and return its number, or return the existing type number if already
   present.  */

static uint32_t
get_func_id (dw_die_ref die)
{
  const char *name = get_AT_string (die, DW_AT_name);
  dw_die_ref spec = get_AT_ref (die, DW_AT_specification);
  bool do_mfunc_id = false;
  codeview_type **slot, *t;
  uint32_t num;

  if (!name)
    return 0;

  if (!func_htab)
    func_htab = new hash_table<die_hasher> (10);

  slot = func_htab->find_slot_with_hash (die, htab_hash_pointer (die), INSERT);

  if (slot && *slot)
    return (*slot)->num;

  if (spec && dw_get_die_parent (spec))
    {
      switch (dw_get_die_tag (dw_get_die_parent (spec)))
	{
	case DW_TAG_class_type:
	case DW_TAG_structure_type:
	case DW_TAG_union_type:
	  do_mfunc_id = true;
	  break;

	default:
	  break;
	}
    }

  if (do_mfunc_id)
    num = add_lf_mfunc_id (die, name);
  else
    num = add_lf_func_id (die, name);

  t = (codeview_type *) xmalloc (sizeof (codeview_type));

  t->die = die;
  t->num = num;
  t->is_fwd_ref = false;

  *slot = t;

  return num;
}

/* Process a DW_TAG_subprogram DIE, and add an S_GPROC32_ID or S_LPROC32_ID
   symbol for this.  */

static void
add_function (dw_die_ref die)
{
  uint32_t func_id_type;
  codeview_symbol *s;

  func_id_type = get_func_id (die);
  if (func_id_type == 0)
    return;

  /* Add an S_GPROC32_ID / S_LPROC32_ID symbol.  */

  s = (codeview_symbol *) xmalloc (sizeof (codeview_symbol));

  s->next = NULL;
  s->kind = get_AT (die, DW_AT_external) ? S_GPROC32_ID : S_LPROC32_ID;
  s->function.parent = 0;
  s->function.end = 0;
  s->function.next = 0;
  s->function.type = func_id_type;
  s->function.flags = 0;
  s->function.name = get_name (die);
  s->function.die = die;

  if (last_sym)
    last_sym->next = s;
  else
    sym = s;

  last_sym = s;
}

/* If we have encountered a new inlined function, add this to
   inlinee_lines_htab so that it can be output to the S_INLINEELINES subsection
   of .debug$S.  */

void
codeview_abstract_function (tree decl)
{
  codeview_inlinee_lines *il, **slot;
  dw_die_ref die;
  uint32_t func_id;
  struct dwarf_file_data *file;

  if (!DECL_DECLARED_INLINE_P (decl))
    return;

  die = lookup_decl_die (decl);
  if (!die)
    return;

  func_id = get_func_id (die);
  if (func_id == 0)
    return;

  file = get_AT_file (die, DW_AT_decl_file);
  if (!file)
    return;

  if (!inlinee_lines_htab)
    inlinee_lines_htab = new hash_table<inlinee_lines_hasher> (10);

  slot = inlinee_lines_htab->find_slot_with_hash (func_id, func_id, INSERT);
  if (*slot)
    return;

  il = (codeview_inlinee_lines *) xmalloc (sizeof (codeview_inlinee_lines));

  il->next = NULL;
  il->func_id = func_id;
  il->file_id = get_file_id (file->filename);
  il->starting_line = get_AT_unsigned (die, DW_AT_decl_line);

  *slot = il;
}

/* Loop through the DIEs that have been output for our TU, and add CodeView
   symbols for them.  */

void
codeview_debug_early_finish (dw_die_ref die)
{
  dw_die_ref first_child, c;

  first_child = dw_get_die_child (die);

  if (!first_child)
    return;

  c = first_child;

  do
    {
      switch (dw_get_die_tag (c))
	{
	case DW_TAG_variable:
	  add_variable (c);
	  break;
	case DW_TAG_subprogram:
	  add_function (c);
	  break;
	default:
	  break;
	}

      c = dw_get_die_sib (c);
    }
  while (c != first_child);

  flush_deferred_types ();
}

#endif
