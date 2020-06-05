
/* Compiler implementation of the D programming language
 * Copyright (C) 2010-2020 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/imphint.c
 */


#include "root/dsystem.h"

#include "mars.h"

/******************************************
 * Looks for undefined identifier s to see
 * if it might be undefined because an import
 * was not specified.
 * Not meant to be a comprehensive list of names in each module,
 * just the most common ones.
 */

const char *importHint(const char *s)
{
    static const char *modules[] =
    {   "core.stdc.stdio",
        "std.stdio",
        "std.math",
        NULL
    };
    static const char *names[] =
    {
        "printf", NULL,
        "writeln", NULL,
        "sin", "cos", "sqrt", "fabs", NULL,
    };
    int m = 0;
    for (int n = 0; modules[m]; n++)
    {
        const char *p = names[n];
        if (p == NULL)
        {
            m++;
            continue;
        }
        assert(modules[m]);
        if (strcmp(s, p) == 0)
            return modules[m];
    }
    return NULL;        // didn't find it
}
