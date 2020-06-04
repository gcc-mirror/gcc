
/* Copyright (C) 1999-2020 by The D Language Foundation, All Rights Reserved
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE or copy at http://www.boost.org/LICENSE_1_0.txt)
 * https://github.com/D-Programming-Language/dmd/blob/master/src/root/file.c
 */

#include "dsystem.h"
#include "file.h"

#if _WIN32
#include <windows.h>
#endif

#if POSIX
#include <utime.h>
#endif

#include "filename.h"
#include "array.h"
#include "rmem.h"

/****************************** File ********************************/

File::File(const FileName *n)
{
    ref = 0;
    buffer = NULL;
    len = 0;
    name = const_cast<FileName *>(n);
}

File *File::create(const char *n)
{
    return new File(n);
}

File::File(const char *n)
{
    ref = 0;
    buffer = NULL;
    len = 0;
    name = new FileName(n);
}

File::~File()
{
    if (buffer)
    {
        if (ref == 0)
            mem.xfree(buffer);
#if _WIN32
        if (ref == 2)
            UnmapViewOfFile(buffer);
#endif
    }
}

/*************************************
 */

bool File::read()
{
    if (len)
        return false;               // already read the file
#if POSIX
    size_t size;
    struct stat buf;
    ssize_t numread;

    const char *name = this->name->toChars();
    //printf("File::read('%s')\n",name);
    int fd = open(name, O_RDONLY);
    if (fd == -1)
    {
        //printf("\topen error, errno = %d\n",errno);
        goto err1;
    }

    if (!ref)
        ::free(buffer);
    ref = 0;       // we own the buffer now

    //printf("\tfile opened\n");
    if (fstat(fd, &buf))
    {
        printf("\tfstat error, errno = %d\n",errno);
        goto err2;
    }
    size = (size_t)buf.st_size;
#ifdef IN_GCC
    buffer = (unsigned char *) ::xmalloc(size + 2);
#else
    buffer = (unsigned char *) ::malloc(size + 2);
#endif
    if (!buffer)
    {
        printf("\tmalloc error, errno = %d\n",errno);
        goto err2;
    }

    numread = ::read(fd, buffer, size);
    if (numread != (ssize_t)size)
    {
        printf("\tread error, errno = %d\n",errno);
        goto err2;
    }

    if (close(fd) == -1)
    {
        printf("\tclose error, errno = %d\n",errno);
        goto err;
    }

    len = size;

    // Always store a wchar ^Z past end of buffer so scanner has a sentinel
    buffer[size] = 0;           // ^Z is obsolete, use 0
    buffer[size + 1] = 0;
    return false;

err2:
    close(fd);
err:
    ::free(buffer);
    buffer = NULL;
    len = 0;

err1:
    return true;
#elif _WIN32
    DWORD size;
    DWORD numread;

    const char *name = this->name->toChars();
    HANDLE h = CreateFileA(name,GENERIC_READ,FILE_SHARE_READ,NULL,OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN,NULL);
    if (h == INVALID_HANDLE_VALUE)
        goto err1;

    if (!ref)
        ::free(buffer);
    ref = 0;

    size = GetFileSize(h,NULL);
#ifdef IN_GCC
    buffer = (unsigned char *) ::xmalloc(size + 2);
#else
    buffer = (unsigned char *) ::malloc(size + 2);
#endif
    if (!buffer)
        goto err2;

    if (ReadFile(h,buffer,size,&numread,NULL) != TRUE)
        goto err2;

    if (numread != size)
        goto err2;

    if (!CloseHandle(h))
        goto err;

    len = size;

    // Always store a wchar ^Z past end of buffer so scanner has a sentinel
    buffer[size] = 0;           // ^Z is obsolete, use 0
    buffer[size + 1] = 0;
    return 0;

err2:
    CloseHandle(h);
err:
    ::free(buffer);
    buffer = NULL;
    len = 0;

err1:
    return true;
#else
    assert(0);
#endif
}

/*********************************************
 * Write a file.
 * Returns:
 *      false       success
 */

bool File::write()
{
#if POSIX
    ssize_t numwritten;

    const char *name = this->name->toChars();
    int fd = open(name, O_CREAT | O_WRONLY | O_TRUNC, (6 << 6) | (4 << 3) | 4);
    if (fd == -1)
        goto err;

    numwritten = ::write(fd, buffer, len);
    if ((ssize_t)len != numwritten)
        goto err2;

    if (close(fd) == -1)
        goto err;

    return false;

err2:
    close(fd);
    ::remove(name);
err:
    return true;
#elif _WIN32
    DWORD numwritten;

    const char *name = this->name->toChars();
    HANDLE h = CreateFileA(name,GENERIC_WRITE,0,NULL,CREATE_ALWAYS,
        FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN,NULL);
    if (h == INVALID_HANDLE_VALUE)
        goto err;

    if (WriteFile(h,buffer,len,&numwritten,NULL) != TRUE)
        goto err2;

    if (len != numwritten)
        goto err2;

    if (!CloseHandle(h))
        goto err;
    return false;

err2:
    CloseHandle(h);
    DeleteFileA(name);
err:
    return true;
#else
    assert(0);
#endif
}

void File::remove()
{
#if POSIX
    ::remove(this->name->toChars());
#elif _WIN32
    DeleteFileA(this->name->toChars());
#else
    assert(0);
#endif
}

const char *File::toChars()
{
    return name->toChars();
}
