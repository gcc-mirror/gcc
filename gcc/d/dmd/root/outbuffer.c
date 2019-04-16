
/* Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE or copy at http://www.boost.org/LICENSE_1_0.txt)
 * https://github.com/D-Programming-Language/dmd/blob/master/src/root/outbuffer.c
 */

#include "dsystem.h"
#include "outbuffer.h"
#include "object.h"

char *OutBuffer::extractData()
{
    char *p;

    p = (char *)data;
    data = NULL;
    offset = 0;
    size = 0;
    return p;
}

void OutBuffer::reserve(size_t nbytes)
{
    //printf("OutBuffer::reserve: size = %d, offset = %d, nbytes = %d\n", size, offset, nbytes);
    if (size - offset < nbytes)
    {
        size = (offset + nbytes) * 2;
        size = (size + 15) & ~15;
        data = (unsigned char *)mem.xrealloc(data, size);
    }
}

void OutBuffer::reset()
{
    offset = 0;
}

void OutBuffer::setsize(size_t size)
{
    offset = size;
}

void OutBuffer::write(const void *data, size_t nbytes)
{
    if (doindent && !notlinehead)
    {
        if (level)
        {
            reserve(level);
            for (int i = 0; i < level; i++)
            {
                this->data[offset] = '\t';
                offset++;
            }
        }
        notlinehead = 1;
    }
    reserve(nbytes);
    memcpy(this->data + offset, data, nbytes);
    offset += nbytes;
}

void OutBuffer::writebstring(utf8_t *string)
{
    write(string,*string + 1);
}

void OutBuffer::writestring(const char *string)
{
    write(string,strlen(string));
}

void OutBuffer::prependstring(const char *string)
{
    size_t len = strlen(string);
    reserve(len);
    memmove(data + len, data, offset);
    memcpy(data, string, len);
    offset += len;
}

void OutBuffer::writenl()
{
#if _WIN32
    writeword(0x0A0D);          // newline is CR,LF on Microsoft OS's
#else
    writeByte('\n');
#endif
    if (doindent)
        notlinehead = 0;
}

void OutBuffer::writeByte(unsigned b)
{
    if (doindent && !notlinehead
        && b != '\n')
    {
        if (level)
        {
            reserve(level);
            for (int i = 0; i < level; i++)
            {
                this->data[offset] = '\t';
                offset++;
            }
        }
        notlinehead = 1;
    }
    reserve(1);
    this->data[offset] = (unsigned char)b;
    offset++;
}

void OutBuffer::writeUTF8(unsigned b)
{
    reserve(6);
    if (b <= 0x7F)
    {
        this->data[offset] = (unsigned char)b;
        offset++;
    }
    else if (b <= 0x7FF)
    {
        this->data[offset + 0] = (unsigned char)((b >> 6) | 0xC0);
        this->data[offset + 1] = (unsigned char)((b & 0x3F) | 0x80);
        offset += 2;
    }
    else if (b <= 0xFFFF)
    {
        this->data[offset + 0] = (unsigned char)((b >> 12) | 0xE0);
        this->data[offset + 1] = (unsigned char)(((b >> 6) & 0x3F) | 0x80);
        this->data[offset + 2] = (unsigned char)((b & 0x3F) | 0x80);
        offset += 3;
    }
    else if (b <= 0x1FFFFF)
    {
        this->data[offset + 0] = (unsigned char)((b >> 18) | 0xF0);
        this->data[offset + 1] = (unsigned char)(((b >> 12) & 0x3F) | 0x80);
        this->data[offset + 2] = (unsigned char)(((b >> 6) & 0x3F) | 0x80);
        this->data[offset + 3] = (unsigned char)((b & 0x3F) | 0x80);
        offset += 4;
    }
    else if (b <= 0x3FFFFFF)
    {
        this->data[offset + 0] = (unsigned char)((b >> 24) | 0xF8);
        this->data[offset + 1] = (unsigned char)(((b >> 18) & 0x3F) | 0x80);
        this->data[offset + 2] = (unsigned char)(((b >> 12) & 0x3F) | 0x80);
        this->data[offset + 3] = (unsigned char)(((b >> 6) & 0x3F) | 0x80);
        this->data[offset + 4] = (unsigned char)((b & 0x3F) | 0x80);
        offset += 5;
    }
    else if (b <= 0x7FFFFFFF)
    {
        this->data[offset + 0] = (unsigned char)((b >> 30) | 0xFC);
        this->data[offset + 1] = (unsigned char)(((b >> 24) & 0x3F) | 0x80);
        this->data[offset + 2] = (unsigned char)(((b >> 18) & 0x3F) | 0x80);
        this->data[offset + 3] = (unsigned char)(((b >> 12) & 0x3F) | 0x80);
        this->data[offset + 4] = (unsigned char)(((b >> 6) & 0x3F) | 0x80);
        this->data[offset + 5] = (unsigned char)((b & 0x3F) | 0x80);
        offset += 6;
    }
    else
        assert(0);
}

void OutBuffer::prependbyte(unsigned b)
{
    reserve(1);
    memmove(data + 1, data, offset);
    data[0] = (unsigned char)b;
    offset++;
}

void OutBuffer::writewchar(unsigned w)
{
#if _WIN32
    writeword(w);
#else
    write4(w);
#endif
}

void OutBuffer::writeword(unsigned w)
{
#if _WIN32
    unsigned newline = 0x0A0D;
#else
    unsigned newline = '\n';
#endif
    if (doindent && !notlinehead
        && w != newline)
    {
        if (level)
        {
            reserve(level);
            for (int i = 0; i < level; i++)
            {
                this->data[offset] = '\t';
                offset++;
            }
        }
        notlinehead = 1;
    }
    reserve(2);
    *(unsigned short *)(this->data + offset) = (unsigned short)w;
    offset += 2;
}

void OutBuffer::writeUTF16(unsigned w)
{
    reserve(4);
    if (w <= 0xFFFF)
    {
        *(unsigned short *)(this->data + offset) = (unsigned short)w;
        offset += 2;
    }
    else if (w <= 0x10FFFF)
    {
        *(unsigned short *)(this->data + offset) = (unsigned short)((w >> 10) + 0xD7C0);
        *(unsigned short *)(this->data + offset + 2) = (unsigned short)((w & 0x3FF) | 0xDC00);
        offset += 4;
    }
    else
        assert(0);
}

void OutBuffer::write4(unsigned w)
{
#if _WIN32
    bool notnewline = w != 0x000A000D;
#else
    bool notnewline = true;
#endif
    if (doindent && !notlinehead && notnewline)
    {
        if (level)
        {
            reserve(level);
            for (int i = 0; i < level; i++)
            {
                this->data[offset] = '\t';
                offset++;
            }
        }
        notlinehead = 1;
    }
    reserve(4);
    *(unsigned *)(this->data + offset) = w;
    offset += 4;
}

void OutBuffer::write(OutBuffer *buf)
{
    if (buf)
    {   reserve(buf->offset);
        memcpy(data + offset, buf->data, buf->offset);
        offset += buf->offset;
    }
}

void OutBuffer::write(RootObject *obj)
{
    if (obj)
    {
        writestring(obj->toChars());
    }
}

void OutBuffer::fill0(size_t nbytes)
{
    reserve(nbytes);
    memset(data + offset,0,nbytes);
    offset += nbytes;
}

void OutBuffer::vprintf(const char *format, va_list args)
{
    int count;

    if (doindent)
        write(NULL, 0); // perform indent
    int psize = 128;
    for (;;)
    {
        reserve(psize);
#if _WIN32
        count = _vsnprintf((char *)data + offset,psize,format,args);
        if (count != -1)
            break;
        psize *= 2;
#elif POSIX
        va_list va;
        va_copy(va, args);
/*
  The functions vprintf(), vfprintf(), vsprintf(), vsnprintf()
  are equivalent to the functions printf(), fprintf(), sprintf(),
  snprintf(), respectively, except that they are called with a
  va_list instead of a variable number of arguments. These
  functions do not call the va_end macro. Consequently, the value
  of ap is undefined after the call. The application should call
  va_end(ap) itself afterwards.
 */
        count = vsnprintf((char *)data + offset,psize,format,va);
        va_end(va);
        if (count == -1)
            psize *= 2;
        else if (count >= psize)
            psize = count + 1;
        else
            break;
#else
        assert(0);
#endif
    }
    offset += count;
}

void OutBuffer::printf(const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    vprintf(format,ap);
    va_end(ap);
}

void OutBuffer::bracket(char left, char right)
{
    reserve(2);
    memmove(data + 1, data, offset);
    data[0] = left;
    data[offset + 1] = right;
    offset += 2;
}

/******************
 * Insert left at i, and right at j.
 * Return index just past right.
 */

size_t OutBuffer::bracket(size_t i, const char *left, size_t j, const char *right)
{
    size_t leftlen = strlen(left);
    size_t rightlen = strlen(right);
    reserve(leftlen + rightlen);
    insert(i, left, leftlen);
    insert(j + leftlen, right, rightlen);
    return j + leftlen + rightlen;
}

void OutBuffer::spread(size_t offset, size_t nbytes)
{
    reserve(nbytes);
    memmove(data + offset + nbytes, data + offset,
        this->offset - offset);
    this->offset += nbytes;
}

/****************************************
 * Returns: offset + nbytes
 */

size_t OutBuffer::insert(size_t offset, const void *p, size_t nbytes)
{
    spread(offset, nbytes);
    memmove(data + offset, p, nbytes);
    return offset + nbytes;
}

void OutBuffer::remove(size_t offset, size_t nbytes)
{
    memmove(data + offset, data + offset + nbytes, this->offset - (offset + nbytes));
    this->offset -= nbytes;
}

char *OutBuffer::peekString()
{
    if (!offset || data[offset-1] != '\0')
    {
        writeByte(0);
        offset--; // allow appending more
    }
    return (char *)data;
}

char *OutBuffer::extractString()
{
    if (!offset || data[offset-1] != '\0')
        writeByte(0);
    return extractData();
}
