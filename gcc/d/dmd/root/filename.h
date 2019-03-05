
/* Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/root/filename.h
 */

#pragma once

#include "array.h"

class RootObject;

template <typename TYPE> struct Array;
typedef Array<const char *> Strings;

struct FileName
{
    const char *str;
    FileName(const char *str);
    bool equals(RootObject *obj);
    static bool equals(const char *name1, const char *name2);
    int compare(RootObject *obj);
    static int compare(const char *name1, const char *name2);
    static bool absolute(const char *name);
    static const char *ext(const char *);
    const char *ext();
    static const char *removeExt(const char *str);
    static const char *name(const char *);
    const char *name();
    static const char *path(const char *);
    static const char *replaceName(const char *path, const char *name);

    static const char *combine(const char *path, const char *name);
    static Strings *splitPath(const char *path);
    static const char *defaultExt(const char *name, const char *ext);
    static const char *forceExt(const char *name, const char *ext);
    static bool equalsExt(const char *name, const char *ext);

    bool equalsExt(const char *ext);

    static const char *searchPath(Strings *path, const char *name, bool cwd);
    static const char *safeSearchPath(Strings *path, const char *name);
    static int exists(const char *name);
    static bool ensurePathExists(const char *path);
    static const char *canonicalName(const char *name);

    static void free(const char *str);
    const char *toChars() const;
};
