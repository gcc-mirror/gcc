
/* Copyright (C) 1999-2022 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
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
