
/* Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/common/outbuffer.h
 */

#pragma once

#include "../root/dsystem.h"
#include "../root/dcompat.h"
#include "../root/rmem.h"

class RootObject;

struct OutBuffer
{
    // IMPORTANT: PLEASE KEEP STATE AND DESTRUCTOR IN SYNC WITH DEFINITION IN ./outbuffer.d.
private:
    DArray<unsigned char> data;
    d_size_t offset;
    d_bool notlinehead;
    void *fileMapping;  // pointer to a file mapping object not used on the C++ side
public:
    d_bool doindent;
    d_bool spaces;
    int level;

    OutBuffer()
    {
        data = DArray<unsigned char>();
        offset = 0;

        doindent = 0;
        level = 0;
        notlinehead = 0;
        fileMapping = 0;
    }
    ~OutBuffer()
    {
        mem.xfree(data.ptr);
    }
    d_size_t length() const { return offset; }
    char *extractData();
    void destroy();

    void reserve(d_size_t nbytes);
    void setsize(d_size_t size);
    void reset();
    void write(const void *data, d_size_t nbytes);
    void writestring(const char *string);
    void prependstring(const char *string);
    void writenl();                     // write newline
    void writeByte(unsigned b);
    void writeUTF8(unsigned b);
    void prependbyte(unsigned b);
    void writewchar(unsigned w);
    void writeword(unsigned w);
    void writeUTF16(unsigned w);
    void write4(unsigned w);
    void write(const OutBuffer *buf);
    void write(RootObject *obj);
    void fill0(d_size_t nbytes);
    void vprintf(const char *format, va_list args);
    void printf(const char *format, ...);
    void bracket(char left, char right);
    d_size_t bracket(d_size_t i, const char *left, d_size_t j, const char *right);
    void spread(d_size_t offset, d_size_t nbytes);
    d_size_t insert(d_size_t offset, const void *data, d_size_t nbytes);
    void remove(d_size_t offset, d_size_t nbytes);
    // Append terminating null if necessary and get view of internal buffer
    char *peekChars();
    // Append terminating null if necessary and take ownership of data
    char *extractChars();
};
