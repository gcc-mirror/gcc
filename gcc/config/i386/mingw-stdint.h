/* Definitions for <stdint.h> types on systems using mingw.
   Copyright (C) 2009-2022 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define SIG_ATOMIC_TYPE "int"

#define INT8_TYPE "signed char"
#define INT16_TYPE "short int"
#define INT32_TYPE "int"
#define INT64_TYPE "long long int"
#define UINT8_TYPE "unsigned char"
#define UINT16_TYPE "short unsigned int"
#define UINT32_TYPE "unsigned int"
#define UINT64_TYPE "long long unsigned int"

#define INT_LEAST8_TYPE "signed char"
#define INT_LEAST16_TYPE "short int"
#define INT_LEAST32_TYPE "int"
#define INT_LEAST64_TYPE "long long int"
#define UINT_LEAST8_TYPE "unsigned char"
#define UINT_LEAST16_TYPE "short unsigned int"
#define UINT_LEAST32_TYPE "unsigned int"
#define UINT_LEAST64_TYPE "long long unsigned int"

#define INT_FAST8_TYPE "signed char"
#define INT_FAST16_TYPE "short int"
#define INT_FAST32_TYPE "int"
#define INT_FAST64_TYPE "long long int"
#define UINT_FAST8_TYPE "unsigned char"
#define UINT_FAST16_TYPE "short unsigned int"
#define UINT_FAST32_TYPE "unsigned int"
#define UINT_FAST64_TYPE "long long unsigned int"

#define INTPTR_TYPE (TARGET_64BIT ? "long long int" : "int")
#define UINTPTR_TYPE (TARGET_64BIT ? "long long unsigned int" : "unsigned int")
