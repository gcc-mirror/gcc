/* malloc.h -- Public #include File (module.h template V1.0)
   Copyright (C) 1995 Free Software Foundation, Inc.
   Contributed by James Craig Burley.

This file is part of GNU Fortran.

GNU Fortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Fortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.

   Owning Modules:
      malloc.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef GCC_F_MALLOC_H
#define GCC_F_MALLOC_H

#ifndef MALLOC_DEBUG
#define MALLOC_DEBUG 0	/* 1 means check caller's use of this module. */
#endif

/* Simple definitions and enumerations. */

typedef enum
  {
    MALLOC_typeKS_,
    MALLOC_typeKSR_,
    MALLOC_typeKP_,
    MALLOC_typeKPR_,
    MALLOC_typeUS_,
    MALLOC_typeUSR_,
    MALLOC_type_
  } mallocType_;

/* Typedefs. */

typedef struct _malloc_area_ *mallocArea_;
typedef struct _malloc_pool_ *mallocPool;
typedef unsigned long int mallocSize;
#define mallocSize_f "l"

/* Include files needed by this one. */


/* Structure definitions. */

struct _malloc_area_
  {
    mallocArea_ next;
    mallocArea_ previous;
    void *where;
#if MALLOC_DEBUG
    mallocSize size;
    mallocType_ type;
#endif
    char name[1];
  };

struct _malloc_pool_
  {
    mallocPool next;
    mallocPool previous;
    mallocPool eldest;
    mallocPool youngest;
    mallocArea_ first;
    mallocArea_ last;
    unsigned long uses;
#if MALLOC_DEBUG
    mallocSize allocated;
    mallocSize freed;
    mallocSize old_sizes;
    mallocSize new_sizes;
    unsigned long allocations;
    unsigned long frees;
    unsigned long resizes;
#endif
    char name[1];
  };

struct _malloc_root_
  {
    struct _malloc_pool_ malloc_pool_image_;
  };

/* Global objects accessed by users of this module. */

extern struct _malloc_root_ malloc_root_;

/* Declare functions with prototypes. */

void malloc_display_ (mallocArea_ a);
mallocArea_ malloc_find_inpool_ (mallocPool pool, void *ptr);
void malloc_init (void);
void malloc_kill_inpool_ (mallocPool pool, mallocType_ type, void *ptr,
			  mallocSize size);
void *malloc_new_ (mallocSize size);
void *malloc_new_inpool_ (mallocPool pool, mallocType_ type, const char *name,
			  mallocSize size);
void *malloc_new_zinpool_ (mallocPool pool, mallocType_ type, const char *name,
			   mallocSize size, int z);
void malloc_pool_display (mallocPool p);
char malloc_pool_find_ (mallocPool p, mallocPool parent);
void malloc_pool_kill (mallocPool p);
mallocPool malloc_pool_new (const char *name, mallocPool parent, unsigned long chunks);
mallocPool malloc_pool_use (mallocPool p);
void *malloc_resize_ (void *ptr, mallocSize new_size);
void *malloc_resize_inpool_ (mallocPool pool, mallocType_ type, void *ptr,
			     mallocSize new_size, mallocSize old_size);
void malloc_verify_inpool_ (mallocPool pool, mallocType_ type, void *ptr,
			    mallocSize size);

/* Define macros. */

#define malloc_new_ks(pool,name,size) \
  malloc_new_inpool_ (pool,MALLOC_typeKS_,name,size)
#define malloc_new_ksr(pool,name,size) \
  malloc_new_inpool_ (pool,MALLOC_typeKSR_,name,size)
#define malloc_new_kp(pool,name,size) \
  malloc_new_inpool_ (pool,MALLOC_typeKP_,name,size)
#define malloc_new_kpr(pool,name,size) \
  malloc_new_inpool_ (pool,MALLOC_typeKPR_,name,size)
#define malloc_new_us(pool,name,size) \
  malloc_new_inpool_ (pool,MALLOC_typeUS_,name,size)
#define malloc_new_usr(pool,name,size) \
  malloc_new_inpool_ (pool,MALLOC_typeUSR_,name,size)
#define malloc_new_zks(pool,name,size,z) \
  malloc_new_zinpool_ (pool,MALLOC_typeKS_,name,size,z)
#define malloc_new_zksr(pool,name,size,z) \
  malloc_new_zinpool_ (pool,MALLOC_typeKSR_,name,size,z)
#define malloc_new_zkp(pool,name,size,z) \
  malloc_new_zinpool_ (pool,MALLOC_typeKP_,name,size,z)
#define malloc_new_zkpr(pool,name,size,z) \
  malloc_new_zinpool_ (pool,MALLOC_typeKPR_,name,size,z)
#define malloc_new_zus(pool,name,size,z) \
  malloc_new_zinpool_ (pool,MALLOC_typeUS_,name,size,z)
#define malloc_new_zusr(pool,name,size,z) \
  malloc_new_zinpool_ (pool,MALLOC_typeUSR_,name,size,z)
#define malloc_kill_ks(pool,ptr,size) \
  malloc_kill_inpool_ (pool,MALLOC_typeKS_,ptr,size)
#define malloc_kill_ksr(pool,ptr,size) \
  malloc_kill_inpool_ (pool,MALLOC_typeKSR_,ptr,size)
#define malloc_kill_us(pool,ptr) \
  malloc_kill_inpool_ (pool,MALLOC_typeUS_,ptr,0)
#define malloc_kill_usr(pool,ptr) \
  malloc_kill_inpool_ (pool,MALLOC_typeUSR_,ptr,0)
#define malloc_pool_image() (&malloc_root_.malloc_pool_image_)
#define malloc_resize_ksr(pool,ptr,new_size,old_size) \
  malloc_resize_inpool_ (pool,MALLOC_typeKSR_,ptr,new_size,old_size)
#define malloc_resize_kpr(pool,ptr,new_size,old_size) \
  malloc_resize_inpool_ (pool,MALLOC_typeKPR_,ptr,new_size,old_size)
#define malloc_resize_usr(pool,ptr,new_size) \
  malloc_resize_inpool_ (pool,MALLOC_typeUSR_,ptr,new_size,0)
#define malloc_verify_kp(pool,name,size) \
  malloc_verify_inpool_ (pool,MALLOC_typeKP_,name,size)
#define malloc_verify_kpr(pool,name,size) \
  malloc_verify_inpool_ (pool,MALLOC_typeKPR_,name,size)
#define malloc_verify_ks(pool,ptr,size) \
  malloc_verify_inpool_ (pool,MALLOC_typeKS_,ptr,size)
#define malloc_verify_ksr(pool,ptr,size) \
  malloc_verify_inpool_ (pool,MALLOC_typeKSR_,ptr,size)
#define malloc_verify_us(pool,ptr) \
  malloc_verify_inpool_ (pool,MALLOC_typeUS_,ptr,0)
#define malloc_verify_usr(pool,ptr) \
  malloc_verify_inpool_ (pool,MALLOC_typeUSR_,ptr,0)

/* End of #include file. */

#endif /* ! GCC_F_MALLOC_H */
