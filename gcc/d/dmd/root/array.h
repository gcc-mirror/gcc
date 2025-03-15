/* Copyright (C) 2011-2025 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/root/array.h
 */

#pragma once

#include "dsystem.h"
#include "rmem.h"

template <typename TYPE>
struct Array
{
    d_size_t length;

  private:
    DArray<TYPE> data;
    #define SMALLARRAYCAP       1
    TYPE smallarray[SMALLARRAYCAP];    // inline storage for small arrays

    Array(const Array&);

  public:
    Array()
    {
        data.ptr = nullptr;
        length = 0;
        data.length = 0;
    }

    ~Array()
    {
        if (data.ptr != &smallarray[0])
            mem.xfree(data.ptr);
    }

    char *toChars() const
    {
        const char **buf = (const char **)mem.xmalloc(length * sizeof(const char *));
        d_size_t len = 2;
        for (d_size_t u = 0; u < length; u++)
        {
            buf[u] = ((TYPE)data.ptr[u])->toChars();
            len += strlen(buf[u]) + 1;
        }
        char *str = (char *)mem.xmalloc(len);

        str[0] = '[';
        char *p = str + 1;
        for (d_size_t u = 0; u < length; u++)
        {
            if (u)
                *p++ = ',';
            len = strlen(buf[u]);
            memcpy(p,buf[u],len);
            p += len;
        }
        *p++ = ']';
        *p = 0;
        mem.xfree(buf);
        return str;
    }

    void push(TYPE ptr)
    {
        reserve(1);
        data.ptr[length++] = ptr;
    }

    void append(Array *a)
    {
        insert(length, a);
    }

    void reserve(d_size_t nentries)
    {
        //printf("Array::reserve: length = %d, data.length = %d, nentries = %d\n", (int)length, (int)data.length, (int)nentries);
        if (data.length - length < nentries)
        {
            if (data.length == 0)
            {
                // Not properly initialized, someone memset it to zero
                if (nentries <= SMALLARRAYCAP)
                {
                    data.length = SMALLARRAYCAP;
                    data.ptr = SMALLARRAYCAP ? &smallarray[0] : nullptr;
                }
                else
                {
                    data.length = nentries;
                    data.ptr = (TYPE *)mem.xmalloc(data.length * sizeof(TYPE));
                }
            }
            else if (data.length == SMALLARRAYCAP)
            {
                data.length = length + nentries;
                data.ptr = (TYPE *)mem.xmalloc(data.length * sizeof(TYPE));
                memcpy(data.ptr, &smallarray[0], length * sizeof(TYPE));
            }
            else
            {
                /* Increase size by 1.5x to avoid excessive memory fragmentation
                 */
                d_size_t increment = length / 2;
                if (nentries > increment)       // if 1.5 is not enough
                    increment = nentries;
                data.length = length + increment;
                data.ptr = (TYPE *)mem.xrealloc(data.ptr, data.length * sizeof(TYPE));
            }
        }
    }

    void remove(d_size_t i)
    {
        if (length - i - 1)
            memmove(data.ptr + i, data.ptr + i + 1, (length - i - 1) * sizeof(TYPE));
        length--;
    }

    void insert(d_size_t index, Array *a)
    {
        if (a)
        {
            d_size_t d = a->length;
            reserve(d);
            if (length != index)
                memmove(data.ptr + index + d, data.ptr + index, (length - index) * sizeof(TYPE));
            memcpy(data.ptr + index, a->data.ptr, d * sizeof(TYPE));
            length += d;
        }
    }

    void insert(d_size_t index, TYPE ptr)
    {
        reserve(1);
        memmove(data.ptr + index + 1, data.ptr + index, (length - index) * sizeof(TYPE));
        data.ptr[index] = ptr;
        length++;
    }

    void setDim(d_size_t newdim)
    {
        if (length < newdim)
        {
            reserve(newdim - length);
        }
        length = newdim;
    }

    d_size_t find(TYPE ptr) const
    {
        for (d_size_t i = 0; i < length; i++)
        {
            if (data.ptr[i] == ptr)
                return i;
        }
        return SIZE_MAX;
    }

    bool contains(TYPE ptr) const
    {
        return find(ptr) != SIZE_MAX;
    }

    TYPE& operator[] (d_size_t index)
    {
#ifdef DEBUG
        assert(index < length);
#endif
        return data.ptr[index];
    }

    TYPE *tdata()
    {
        return data.ptr;
    }

    Array *copy()
    {
        Array *a = new Array();
        a->setDim(length);
        memcpy(a->data.ptr, data.ptr, length * sizeof(TYPE));
        return a;
    }

    void shift(TYPE ptr)
    {
        reserve(1);
        memmove(data.ptr + 1, data.ptr, length * sizeof(TYPE));
        data.ptr[0] = ptr;
        length++;
    }

    void zero()
    {
        memset(data.ptr, 0, length * sizeof(TYPE));
    }

    TYPE pop()
    {
        return data.ptr[--length];
    }
};
