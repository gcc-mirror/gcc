
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2018 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/unittests.c
 */

#include <stdio.h>

#include "mars.h"

void unittest_speller();
void unittest_importHint();
void unittest_aa();

void unittests()
{
#if UNITTEST
    unittest_speller();
    unittest_importHint();
    unittest_aa();
#endif
}
