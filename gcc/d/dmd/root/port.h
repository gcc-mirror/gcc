
/* Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/root/port.h
 */

#pragma once

// Portable wrapper around compiler/system specific things.
// The idea is to minimize #ifdef's in the app code.

#include "dsystem.h"
#include "dcompat.h"

typedef unsigned char utf8_t;

struct Port
{
    static int memicmp(const char *s1, const char *s2, d_size_t n);
    static char *strupr(char *s);

    static bool isFloat32LiteralOutOfRange(const char *s);
    static bool isFloat64LiteralOutOfRange(const char *s);

    static void writelongLE(unsigned value, void *buffer);
    static unsigned readlongLE(const void *buffer);
    static void writelongBE(unsigned value, void *buffer);
    static unsigned readlongBE(const void *buffer);
    static unsigned readwordLE(const void *buffer);
    static unsigned readwordBE(const void *buffer);
    static void valcpy(void *dst, uint64_t val, d_size_t size);
};
