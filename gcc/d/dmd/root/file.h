
/* Copyright (C) 1999-2022 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/root/file.h
 */

#pragma once

#include "array.h"
#include "filename.h"

struct FileBuffer
{
    DArray<unsigned char> data;

    FileBuffer(const FileBuffer &) /* = delete */;
    ~FileBuffer() { mem.xfree(data.ptr); }

    static FileBuffer *create();
};

struct File
{
    struct ReadResult
    {
        bool success;
        FileBuffer buffer;
    };

    // Read the full content of a file.
    static ReadResult read(const char *name);

    // Write a file, returning `true` on success.
    static bool write(const char *name, const void *data, d_size_t size);

    // Delete a file.
    static void remove(const char *name);
};
