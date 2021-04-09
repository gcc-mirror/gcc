
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/identifier.h
 */

#pragma once

#include "root/root.h"
#include "root/stringtable.h"

class Identifier : public RootObject
{
private:
    int value;
    const char *string;
    size_t len;

public:
    Identifier(const char *string, size_t length, int value);
    Identifier(const char *string);
    static Identifier* create(const char *string);
    bool equals(RootObject *o);
    int compare(RootObject *o);
    void print();
    const char *toChars();
    int getValue() const;
    const char *toHChars2();
    int dyncast() const;

    static StringTable stringtable;
    static Identifier *generateId(const char *prefix);
    static Identifier *generateId(const char *prefix, size_t i);
    static Identifier *idPool(const char *s, size_t len);
    static Identifier *idPool(const char *s, size_t len, int value);

    static inline Identifier *idPool(const char *s)
    {
        return idPool(s, strlen(s));
    }

    static bool isValidIdentifier(const char *p);
    static Identifier *lookup(const char *s, size_t len);
    static void initTable();
};
