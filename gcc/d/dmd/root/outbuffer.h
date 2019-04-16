
/* Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/root/outbuffer.h
 */

#pragma once

#include "dsystem.h"
#include "port.h"
#include "rmem.h"

class RootObject;

struct OutBuffer
{
    unsigned char *data;
    size_t offset;
    size_t size;

    int level;
    bool doindent;
private:
    bool notlinehead;
public:

    OutBuffer()
    {
        data = NULL;
        offset = 0;
        size = 0;

        doindent = 0;
        level = 0;
        notlinehead = 0;
    }
    ~OutBuffer()
    {
        mem.xfree(data);
    }
    char *extractData();

    void reserve(size_t nbytes);
    void setsize(size_t size);
    void reset();
    void write(const void *data, d_size_t nbytes);
    void writebstring(utf8_t *string);
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
    void write(OutBuffer *buf);
    void write(RootObject *obj);
    void fill0(size_t nbytes);
    void vprintf(const char *format, va_list args);
    void printf(const char *format, ...);
    void bracket(char left, char right);
    size_t bracket(size_t i, const char *left, size_t j, const char *right);
    void spread(size_t offset, size_t nbytes);
    size_t insert(size_t offset, const void *data, size_t nbytes);
    void remove(size_t offset, size_t nbytes);
    // Append terminating null if necessary and get view of internal buffer
    char *peekString();
    // Append terminating null if necessary and take ownership of data
    char *extractString();
};
