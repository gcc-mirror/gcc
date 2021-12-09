
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/identifier.h
 */

#pragma once

#include "root/dcompat.h"
#include "root/object.h"

class Identifier : public RootObject
{
private:
    int value;
    bool isAnonymous_;
    DString string;

public:
    static Identifier* create(const char *string);
    bool equals(const RootObject *o) const;
    const char *toChars() const;
    int getValue() const;
    bool isAnonymous() const;
    const char *toHChars2() const;
    DYNCAST dyncast() const;

    static Identifier *generateId(const char *prefix, size_t length, size_t suffix);
    static Identifier *idPool(const char *s, unsigned len);

    static inline Identifier *idPool(const char *s)
    {
        return idPool(s, static_cast<unsigned>(strlen(s)));
    }

    static bool isValidIdentifier(const char *p);
};
