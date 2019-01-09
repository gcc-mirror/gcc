
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/macro.h
 */

#pragma once

#include "root/dsystem.h"
#include "root/root.h"


struct Macro
{
  private:
    Macro *next;                // next in list

    const utf8_t *name;        // macro name
    size_t namelen;             // length of macro name

    const utf8_t *text;        // macro replacement text
    size_t textlen;             // length of replacement text

    int inuse;                  // macro is in use (don't expand)

    Macro(const utf8_t *name, size_t namelen, const utf8_t *text, size_t textlen);
    Macro *search(const utf8_t *name, size_t namelen);

  public:
    static Macro *define(Macro **ptable, const utf8_t *name, size_t namelen, const utf8_t *text, size_t textlen);

    void expand(OutBuffer *buf, size_t start, size_t *pend,
        const utf8_t *arg, size_t arglen);
};
