/** Definitions for block support for GNUStep
   Copyright (C) 2011 Free Software Foundation, Inc.

   This file is part of the GNUstep Base Library.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02111 USA.

   */

#ifndef __GSBlocks_h_GNUSTEP_BASE_INCLUDE
#define __GSBlocks_h_GNUSTEP_BASE_INCLUDE

/* Define the has_feature pseudo-macro for GCC. */
#ifndef __has_feature
#define __has_feature(x) 0
#endif

#ifndef GCC_VERSION
#define GCC_VERSION (__GNUC__ * 1000 + __GNUC_MINOR__)
#endif

#if __has_feature(blocks)

#define BLOCK_SCOPE __block
/**
 * Defines a block type.  Will work whether or not the compiler natively
 * supports blocks.
 */
#define DEFINE_BLOCK_TYPE(name, retTy, argTys, ...) \
typedef retTy(^name)(argTys, ## __VA_ARGS__)

#define DEFINE_BLOCK_TYPE_NO_ARGS(name, retTy) \
typedef retTy(^name)()

/**
 * Calls a block.  Works irrespective of whether the compiler supports blocks.
 */
#define CALL_BLOCK(block, args, ...) block(args, ## __VA_ARGS__)

/**
 * Calls a block without arguments.
 */
#define CALL_BLOCK_NO_ARGS(block) block()
#else

/* Fall-back versions for when the compiler doesn't have native blocks support.
 */
#if (GCC_VERSION >= 3000)

#define DEFINE_BLOCK_TYPE(name, retTy, argTys, ...) \
  typedef struct {\
    void *isa;\
    int flags;\
    int reserved;\
    retTy (*invoke)(void*, argTys, ## __VA_ARGS__);\
  } *name

#define DEFINE_BLOCK_TYPE_NO_ARGS(name, retTy) \
  typedef struct {\
    void *isa;\
    int flags;\
    int reserved;\
    retTy (*invoke)(void*);\
  } *name

#define CALL_BLOCK(block, args, ...) block->invoke(block, args, ## __VA_ARGS__)

#define CALL_BLOCK_NO_ARGS(block) block->invoke(block)
#define BLOCK_SCOPE

#else /* GCC_VERSION >= 3000 */

#define DEFINE_BLOCK_TYPE(name, retTy, argTys...) \
  typedef struct {\
    void *isa;\
    int flags;\
    int reserved;\
    retTy (*invoke)(void*, argTys);\
  } *name

#define DEFINE_BLOCK_TYPE_NO_ARGS(name, retTy) \
  typedef struct {\
    void *isa;\
    int flags;\
    int reserved;\
    retTy (*invoke)(void*);\
  } *name


#define CALL_BLOCK(block, args...) block->invoke(block, args)
#define CALL_BLOCK_NO_ARGS(block) block->invoke(block)
#define BLOCK_SCOPE
#endif /* GCC_VERSION >= 3000 */

#endif /* __has_feature(blocks) */

#if __has_include(<objc/blocks_runtime.h>)
#  include <objc/blocks_runtime.h>
#else

#ifdef __cplusplus
extern "C" {
#endif

/**
 * _Block_copy and _Block_release are weakly imported, but can be assumed
 * to be available whenever a feature using blocks is accessed
 * by an application.
 */

/* weak attributed supported only with ELF, MINGW is COFF */
#ifndef __MINGW32__

void *_Block_copy(const void *) __attribute__((weak));
void _Block_release(const void *) __attribute__((weak));

#endif /* __MINGW32__ */

#ifdef __cplusplus
}
#endif

#ifndef Block_copy
#  define Block_copy(x) ((__typeof(x))_Block_copy((const void *)(x)))
#endif
#ifndef Block_release
#  define Block_release(x) _Block_release((const void *)(x))
#endif

#endif /* __has_include(<objc/blocks_runtime.h>) */
#endif /* __GSBlocks_h_GNUSTEP_BASE_INCLUDE */

