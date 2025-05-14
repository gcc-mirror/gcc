
/* Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/rootobject.h
 */

#pragma once

#include "root/dsystem.h"
#include "root/dcompat.h"

typedef size_t hash_t;

struct OutBuffer;

enum DYNCAST
{
    DYNCAST_OBJECT,
    DYNCAST_EXPRESSION,
    DYNCAST_DSYMBOL,
    DYNCAST_TYPE,
    DYNCAST_IDENTIFIER,
    DYNCAST_TUPLE,
    DYNCAST_PARAMETER,
    DYNCAST_STATEMENT,
    DYNCAST_CONDITION,
    DYNCAST_TEMPLATEPARAMETER,
    DYNCAST_INITIALIZER
};

/*
 * Root of our class library.
 */
class RootObject
{
public:
    RootObject() { }

    virtual bool equals(const RootObject * const o) const;

    /**
     * Pretty-print an Object. Useful for debugging the old-fashioned way.
     */
    virtual const char *toChars() const;
    /// This function is `extern(D)` and should not be called from C++,
    /// as the ABI does not match on some platforms
    virtual DString toString();

    /**
     * Used as a replacement for dynamic_cast. Returns a unique number
     * defined by the library user. For Object, the return value is 0.
     */
    virtual DYNCAST dyncast() const;
};
