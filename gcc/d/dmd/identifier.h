
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/identifier.h
 */

#pragma once

#include "root/dcompat.h"
#include "root/object.h"

class Identifier final : public RootObject
{
private:
    int value;
    d_bool isAnonymous_;
    DString string;

public:
    static Identifier* create(const char *string);
    const char *toChars() const override;
    int getValue() const;
    bool isAnonymous() const;
    const char *toHChars2() const;
    DYNCAST dyncast() const override;

    static Identifier *generateId(const char *prefix, size_t length, size_t suffix);
    static Identifier *idPool(const char *s, unsigned len);

    static inline Identifier *idPool(const char *s)
    {
        return idPool(s, static_cast<unsigned>(strlen(s)));
    }

    static bool isValidIdentifier(const char *p);
};
