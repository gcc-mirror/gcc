typedef __SIZE_TYPE__ size_t;
/* Kludge */
#define unsigned
typedef __SIZE_TYPE__ __ssize_t;
#undef unsigned
typedef struct
{
} __mbstate_t;
typedef struct
{
} _G_fpos64_t;
enum
{
  __GCONV_INTERNAL_ERROR
};
typedef int (*__gconv_trans_fct) (struct __gconv_step *,
      size_t *);
typedef int (*__gconv_trans_context_fct) (void *, __const unsigned char *,
       unsigned char *, unsigned char *);
struct __gconv_trans_data
{
};
struct _IO_marker {
};
typedef __ssize_t __io_write_fn (void *__cookie, __const char *__buf,
     size_t __n);
typedef struct blockbox {
} *BBOXPTR, BBOX ;
typedef struct netbox {
} *NBOXPTR, NBOX ;
typedef struct termbox {
    struct termbox *nextterm ;
} *TEBOXPTR, TEBOX ;
typedef struct tilebox {
    TEBOXPTR termsptr ;
}
*TIBOXPTR ,
TIBOX ;
typedef struct cellbox {
    TIBOXPTR tileptr ;
}
*CBOXPTR ,
CBOX ;
typedef struct dimbox {
}
DBOX ;
typedef struct rowbox {
} ROWBOX ;
typedef struct binbox {
}
CHANGRDBOX ;
extern int numcells ;
extern int numterms ;
sortpin()
{
int j , n , cell ;
CBOXPTR ptr ;
TIBOXPTR tile ;
TEBOXPTR term , *xpptr ;
for( cell = 1 ; cell <= numcells + numterms ; cell++ ) {
    if( ( tile = ptr->tileptr ) == (TIBOXPTR) ((void *)0) ) {
    }
    n = 0 ;
    for( term = tile->termsptr ; term != (TEBOXPTR) ((void *)0) ;
      term = term->nextterm ) {
 xpptr[ ++n ] = term ;
    }
    xpptr[ n + 1 ] = (TEBOXPTR) ((void *)0) ;
    ptr->tileptr->termsptr = xpptr[ 1 ] ;
    for( j = 1 ; j <= n ; j++ ) {
    }
}
}

