
/* Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/root/stringtable.h
 */

#pragma once

#include "root.h"
#include "rmem.h"   // for d_size_t

struct StringEntry;

// StringValue is a variable-length structure. It has neither proper c'tors nor a
// factory method because the only thing which should be creating these is StringTable.
struct StringValue
{
    void *ptrvalue;
    size_t length;
    char *lstring() { return (char *)(this + 1); }

    size_t len() const { return length; }
    const char *toDchars() const { return (const char *)(this + 1); }

    StringValue();  // not constructible
};

struct StringTable
{
private:
    StringEntry *table;
    size_t tabledim;

    uint8_t **pools;
    size_t npools;
    size_t nfill;

    size_t count;

public:
    void _init(d_size_t size = 0);
    void reset(d_size_t size = 0);
    ~StringTable();

    StringValue *lookup(const char *s, d_size_t len);
    StringValue *insert(const char *s, size_t len, void *ptrvalue);
    StringValue *update(const char *s, d_size_t len);
    int apply(int (*fp)(StringValue *));

private:
    uint32_t allocValue(const char *p, size_t length, void *ptrvalue);
    StringValue *getValue(uint32_t validx);
    size_t findSlot(hash_t hash, const char *s, size_t len);
    void grow();
};
