
/* Copyright (C) 1999-2020 by The D Language Foundation, All Rights Reserved
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE or copy at http://www.boost.org/LICENSE_1_0.txt)
 * https://github.com/dlang/dmd/blob/master/src/root/object.h
 */

#pragma once

#include "dsystem.h"

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
    DYNCAST_STATEMENT
};

/*
 * Root of our class library.
 */
class RootObject
{
public:
    RootObject() { }

    virtual bool equals(RootObject *o);

    /**
     * Return <0, ==0, or >0 if this is less than, equal to, or greater than obj.
     * Useful for sorting Objects.
     */
    virtual int compare(RootObject *obj);

    /**
     * Pretty-print an Object. Useful for debugging the old-fashioned way.
     */
    virtual void print();

    virtual const char *toChars();
    virtual void toBuffer(OutBuffer *buf);

    /**
     * Used as a replacement for dynamic_cast. Returns a unique number
     * defined by the library user. For Object, the return value is 0.
     */
    virtual int dyncast() const;
};
