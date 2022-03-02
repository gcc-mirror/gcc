/* Shared header for testing memory leak false positives seen in OpenBLAS,
   e.g. in lapacke_cgbbrd_work.c.

   The code is of the form:

   if( LAPACKE_lsame( vect, 'b' ) || LAPACKE_lsame( vect, 'p' ) ) {
            pt_t = (lapack_complex_float*)
                LAPACKE_malloc( sizeof(lapack_complex_float) *
                                ldpt_t * MAX(1,n) );
      ...snip...
   }

   [...snip lots of code...]

   if( LAPACKE_lsame( vect, 'b' ) || LAPACKE_lsame( vect, 'p' ) ) {
            LAPACKE_free( pt_t );
   }

   where LAPACKE_lsame is a case-insensitive comparison, implemented in its
   own source file.  Without __attribute__((const)) on LAPACKE_lsame, the
   analyzer considers the execution paths where the malloc is called, and
   then the free is not called.  With __attribute__((const)), the analyzer
   ought to rule out such paths.  */

#define NULL ((void *)0)
typedef __SIZE_TYPE__ size_t;

extern void *malloc (size_t __size)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__malloc__))
  __attribute__ ((__alloc_size__ (1)))
  __attribute__ ((__warn_unused_result__));
extern void free (void *__ptr)
  __attribute__ ((__nothrow__ , __leaf__));

/* Header adapted/reduced from
     https://github.com/xianyi/OpenBLAS/
   which has this license text.  */

/*****************************************************************************
  Copyright (c) 2014, Intel Corp.
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Intel Corporation nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
  THE POSSIBILITY OF SUCH DAMAGE.
*****************************************************************************/

#define lapack_int     int
#define lapack_logical lapack_int

#define LAPACKE_malloc( size ) malloc( size )
#define LAPACKE_free( p )      free( p )

#define LAPACK_ROW_MAJOR               101
#define LAPACK_COL_MAJOR               102

#define LAPACK_TRANSPOSE_MEMORY_ERROR  -1011

#define MAX(x,y) (((x) > (y)) ? (x) : (y))

#define LAPACK_GLOBAL(lcname,UCNAME)  lcname##_

typedef float _Complex lapack_complex_float;

void LAPACKE_xerbla( const char *name, int info );

void LAPACKE_cgb_trans( int matrix_layout, int m, int n,
                        int kl, int ku,
                        const lapack_complex_float *in, int ldin,
                        lapack_complex_float *out, int ldout );
void LAPACKE_cge_trans( int matrix_layout, int m, int n,
                        const lapack_complex_float* in, int ldin,
                        lapack_complex_float* out, int ldout );

#define LAPACK_cgbbrd LAPACK_GLOBAL(cgbbrd,CGBBRD)
void LAPACK_cgbbrd(
    char const* vect,
    lapack_int const* m, lapack_int const* n, lapack_int const* ncc, lapack_int const* kl, lapack_int const* ku,
    lapack_complex_float* AB, lapack_int const* ldab,
    float* D,
    float* E,
    lapack_complex_float* Q, lapack_int const* ldq,
    lapack_complex_float* PT, lapack_int const* ldpt,
    lapack_complex_float* C, lapack_int const* ldc,
    lapack_complex_float* work,
    float* rwork,
    lapack_int* info );
