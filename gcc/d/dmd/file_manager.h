
/* Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/file_manager.h
 */

#pragma once

#include "root/file.h"

struct FileManager
{
    static void _init();
    FileBuffer* lookup(const char* filename);
    FileBuffer* add(const char* filename, FileBuffer* filebuffer);
};
