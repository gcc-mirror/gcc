/* { dg-do compile }
   { dg-options "-Wall -Wattributes -ftrack-macro-expansion=0" } */

#define ATTR(attrlist) __attribute__ (attrlist)

/* Exercise the handling of the mutually exclusive attributes
   aligned and packed on a type definition.  */

/* Pointless but benign.  */
struct ATTR ((aligned, aligned))
AlignedAligned { int i; };

/* Aligned followed by packed on a type and vice versa has a valid use:
   to decrease the alignment of a type to the specified boundary.  */
struct ATTR ((aligned, packed))
AlignedPacked { int i; };

struct ATTR ((packed, aligned))
PackedAligned { int i; };

struct ATTR ((aligned (2)))
AlignedMemberPacked
{
  int ATTR ((packed)) i;
};

struct ATTR ((packed))
PackedMemberAligned
{
  int ATTR ((aligned (2))) i;
};

/* Silly but benign.  */
struct ATTR ((packed, packed))
PackedPacked { int i; };


/* Exercise the handling of the mutually exclusive attributes
   aligned and packed on a function declaration.  */

void ATTR ((aligned (8), packed))
faligned8_1 (void);           /* { dg-warning "ignoring attribute .packed. because it conflicts with attribute .aligned." } */

void ATTR ((aligned (8)))
faligned8_2 (void);           /* { dg-message "previous declaration here" } */

void ATTR ((packed))
faligned8_2 (void);           /* { dg-warning "ignoring attribute .packed. because it conflicts with attribute .aligned." } */

/* Exercise the handling of the mutually exclusive attributes
   always_inline and noinline (in that order).  */

inline void ATTR ((always_inline))
falways_inline1 (void);

inline void ATTR ((__always_inline__))
falways_inline1 (void);

/* Verify that repeating attribute always_inline on the same declaration
   doesn't trigger a warning.  */
inline void ATTR ((always_inline, __always_inline__))
falways_inline1 (void);

/* Verify that repeating attribute always_inline on a distinct declaration
   doesn't trigger a warning.  */
inline void ATTR ((always_inline))
falways_inline1 (void);       /* { dg-message "previous declaration here" } */

/* Verify a warning for noinline conflict.  */
void ATTR ((noinline))
falways_inline1 (void) { }    /* { dg-warning "ignoring attribute .noinline. because it conflicts with attribute .always_inline." } */

/* And again.  */
void ATTR ((always_inline))
falways_inline1 (void);


/* Exercise the handling of the mutually exclusive attributes
   noinline and always_inline (in that order).  */

void ATTR ((noinline))
fnoinline1 (void);

void ATTR ((__noinline__))
fnoinline1 (void);

/* Verify that repeating attribute noinline on the same declaration
   doesn't trigger a warning.  */
void ATTR ((noinline, __noinline__))
fnoinline1 (void);

/* Verify that repeating attribute noinline on a distinct declaration
   doesn't trigger a warning.  */
void ATTR ((noinline))
fnoinline1 (void);       /* { dg-message "previous declaration here" } */

/* Verify a warning for always_inline conflict.  */
void ATTR ((always_inline))
fnoinline1 (void) { }    /* { dg-warning "ignoring attribute .always_inline. because it conflicts with attribute .noinline." } */

/* Verify a warning for gnu_inline conflict.  */
inline void ATTR ((gnu_inline))
fnoinline1 (void);      /* { dg-warning "ignoring attribute .gnu_inline. because it conflicts with attribute .noinline." } */
                        /* { dg-warning "follows declaration with attribute .noinline." "inline noinline" { target *-*-* } .-1 } */

/* And again.  */
void ATTR ((noinline))
fnoinline1 (void);      /* { dg-warning "follows inline declaration" } */


/* Exercise the handling of the mutually exclusive attributes
   noreturn and warn_unused_result.  */

int ATTR ((__noreturn__))
fnoret1 (void);

int ATTR ((noreturn))
fnoret1 (void);               /* { dg-message "previous declaration here" } */

int ATTR ((warn_unused_result))
fnoret1 (void);               /* { dg-warning "ignoring attribute .warn_unused_result. because it conflicts with attribute .noreturn." } */

/* Verify that repeating attribute noreturn doesn't trigger a warning.  */
int ATTR ((noreturn)) fnoret1 (void);

int call_noret1 (void)
{
  /* Verify that attribute warn_unused_result was, in fact, ignored
     on the second declaration of fnoret1.  */
  fnoret1 ();
}

int ATTR ((noreturn, warn_unused_result))
fnoret2 (void);               /* { dg-warning "ignoring attribute .warn_unused_result. because it conflicts with attribute .noreturn." } */

/* Verify that repeating attribute noreturn doesn't trigger a warning.  */
int ATTR ((noreturn)) fnoret2 (void);

int call_noret2 (void)
{
  /* Verify that attribute warn_unused_result was, in fact, ignored
     on the second declaration of fnoret2.  */
  fnoret2 ();
}

/* Verify that attribute noreturn isn't diagnosed on a declaration
   that was previously declared warn_unused_result and that attribute
   was dropped (because the function returs void).  */

void ATTR ((warn_unused_result))
fnorety6 (void);             /* { dg-warning ".warn_unused_result. attribute ignored" } */

void ATTR ((noreturn))
fnoret6 (void);


/* Exercise the handling of the mutually exclusive attributes
   noreturn and alloc_align.  */

void* ATTR ((noreturn))
fnoret_alloc_align1 (int);    /* { dg-message "previous declaration here" } */

void* ATTR ((alloc_align (1)))
fnoret_alloc_align1 (int);    /* { dg-warning "ignoring attribute .alloc_align. because it conflicts with attribute .noreturn." } */

void* ATTR ((noreturn, alloc_align (1)))
fnoret_alloc_align2 (int);    /* { dg-warning "ignoring attribute .alloc_align. because it conflicts with attribute .noreturn." } */


void* ATTR ((noreturn)) ATTR ((alloc_align (1)))
fnoret_alloc_align3 (int);    /* { dg-warning "ignoring attribute .alloc_align. because it conflicts with attribute .noreturn." } */


void* ATTR ((alloc_align (1)))
falloc_align_noret1 (int);    /* { dg-message "previous declaration here" } */

void* ATTR ((noreturn))
falloc_align_noret1 (int);    /* { dg-warning "ignoring attribute .noreturn. because it conflicts with attribute .alloc_align." } */


void* ATTR ((alloc_align (1), noreturn))
falloc_align_noret2 (int);    /* { dg-warning "ignoring attribute .(noreturn|alloc_align). because it conflicts with attribute .(alloc_align|noreturn)." } */

void* ATTR ((alloc_align (1))) ATTR ((noreturn))
falloc_align_noret3 (int);    /* { dg-warning "ignoring attribute .(noreturn|alloc_align). because it conflicts with attribute .(noreturn|alloc_align)." } */


/* Exercise the handling of the mutually exclusive attributes
   noreturn and alloc_size.  */

void* ATTR ((noreturn))
fnoret_alloc_size1 (int);     /* { dg-message "previous declaration here" } */

void* ATTR ((alloc_size (1)))
fnoret_alloc_size1 (int);     /* { dg-warning "ignoring attribute .alloc_size. because it conflicts with attribute .noreturn." } */

void* ATTR ((noreturn, alloc_size (1)))
fnoret_alloc_size2 (int);     /* { dg-warning "ignoring attribute .alloc_size. because it conflicts with attribute .noreturn." } */


void* ATTR ((noreturn)) ATTR ((alloc_size (1)))
fnoret_alloc_size3 (int);     /* { dg-warning "ignoring attribute .alloc_size. because it conflicts with attribute .noreturn." } */


void* ATTR ((alloc_size (1)))
falloc_size_noret1 (int);     /* { dg-message "previous declaration here" } */

void* ATTR ((noreturn))
falloc_size_noret1 (int);     /* { dg-warning "ignoring attribute .noreturn. because it conflicts with attribute .alloc_size." } */


void* ATTR ((alloc_size (1), noreturn))
falloc_size_noret2 (int);     /* { dg-warning "ignoring attribute .noreturn. because it conflicts with attribute .alloc_size." } */

void* ATTR ((alloc_size (1))) ATTR ((noreturn))
falloc_size_noret3 (int);     /* { dg-warning "ignoring attribute .noreturn. because it conflicts with attribute .alloc_size." } */


/* Exercise the handling of the mutually exclusive attributes
   noreturn and const.  */

int ATTR ((noreturn))
fnoret_const1 (int);          /* { dg-message "previous declaration here" } */

int ATTR ((const))
fnoret_const1 (int);          /* { dg-warning "ignoring attribute .const. because it conflicts with attribute .noreturn." } */

/* Unfortunately, attributes on a single declarations may not be processed
   in the same order as specified... */
int ATTR ((noreturn, const))
fnoret_const2 (int);          /* { dg-warning "ignoring attribute .const. because it conflicts with attribute .noreturn." } */


int ATTR ((noreturn)) ATTR ((const))
fnoret_const3 (int);          /* { dg-warning "ignoring attribute .const. because it conflicts with attribute .noreturn." } */


int ATTR ((const))
fconst_noret1 (int);          /* { dg-message "previous declaration here" } */

int ATTR ((noreturn))
fconst_noret1 (int);          /* { dg-warning "ignoring attribute .noreturn. because it conflicts with attribute .const." } */


int ATTR ((const, noreturn))
fconst_noret2 (int);          /* { dg-warning "ignoring attribute .noreturn. because it conflicts with attribute .const." } */

int ATTR ((const)) ATTR ((noreturn))
fconst_noret3 (int);          /* { dg-warning "ignoring attribute .noreturn. because it conflicts with attribute .const." } */


/* Exercise the handling of the mutually exclusive attributes
   noreturn and malloc.  */

void* ATTR ((noreturn))
fnoret_malloc1 (int);         /* { dg-message "previous declaration here" } */

void* ATTR ((malloc))
fnoret_malloc1 (int);         /* { dg-warning "ignoring attribute .malloc. because it conflicts with attribute .noreturn." } */

/* Unfortunately, attributes on a single declarations may not be processed
   in the same order as specified... */
void* ATTR ((noreturn, malloc))
fnoret_malloc2 (int);         /* { dg-warning "ignoring attribute .malloc. because it conflicts with attribute .noreturn." } */


void* ATTR ((noreturn)) ATTR ((malloc))
fnoret_malloc3 (int);         /* { dg-warning "ignoring attribute .malloc. because it conflicts with attribute .noreturn." } */


void* ATTR ((__malloc__))
fmalloc_noret1 (int);

void* ATTR ((malloc))
fmalloc_noret1 (int);         /* { dg-message "previous declaration here" } */

void* ATTR ((noreturn))
fmalloc_noret1 (int);         /* { dg-warning "ignoring attribute .noreturn. because it conflicts with attribute .malloc." } */


void* ATTR ((malloc, noreturn))
fmalloc_noret2 (int);         /* { dg-warning "ignoring attribute .noreturn. because it conflicts with attribute .malloc." } */

void* ATTR ((malloc)) ATTR ((noreturn))
fmalloc_noret3 (int);         /* { dg-warning "ignoring attribute .noreturn. because it conflicts with attribute .malloc." } */


/* Exercise the handling of the mutually exclusive attributes
   noreturn and pure.  */

int ATTR ((noreturn))
fnoret_pure1 (int);           /* { dg-message "previous declaration here" } */

int ATTR ((pure))
fnoret_pure1 (int);           /* { dg-warning "ignoring attribute .pure. because it conflicts with attribute .noreturn." } */

/* Unfortunately, attributes on a single declarations may not be processed
   in the same order as specified... */
int ATTR ((noreturn, pure))
fnoret_pure2 (int);           /* { dg-warning "ignoring attribute .pure. because it conflicts with attribute .noreturn." } */


int ATTR ((noreturn)) ATTR ((pure))
fnoret_pure3 (int);           /* { dg-warning "ignoring attribute .pure. because it conflicts with attribute .noreturn." } */


int ATTR ((__pure__))
fpure_noret1 (int);

int ATTR ((pure))
fpure_noret1 (int);           /* { dg-message "previous declaration here" } */

int ATTR ((noreturn))
fpure_noret1 (int);           /* { dg-warning "ignoring attribute .noreturn. because it conflicts with attribute .pure." } */


int ATTR ((pure, noreturn))
fpure_noret2 (int);           /* { dg-warning "ignoring attribute .noreturn. because it conflicts with attribute .pur." } */

int ATTR ((pure)) ATTR ((noreturn))
fpure_noret3 (int);            /* { dg-warning "ignoring attribute .noreturn. because it conflicts with attribute .pure." } */


/* Exercise the handling of the mutually exclusive attributes
   noreturn and returns_twice.  */

int ATTR ((noreturn))
fnoret_returns_twice1 (int);  /* { dg-message "previous declaration here" } */

int ATTR ((returns_twice))
fnoret_returns_twice1 (int);  /* { dg-warning "ignoring attribute .returns_twice. because it conflicts with attribute .noreturn." } */

/* Unfortunately, attributes on a single declarations may not be processed
   in the same order as specified... */
int ATTR ((noreturn, returns_twice))
fnoret_returns_twice2 (int);  /* { dg-warning "ignoring attribute .returns_twice. because it conflicts with attribute .noreturn." } */


int ATTR ((noreturn)) ATTR ((returns_twice))
fnoret_returns_twice3 (int);  /* { dg-warning "ignoring attribute .returns_twice. because it conflicts with attribute .noreturn." } */


int ATTR ((returns_twice))
freturns_twice_noret1 (int);  /* { dg-message "previous declaration here" } */

int ATTR ((noreturn))
freturns_twice_noret1 (int);  /* { dg-warning "ignoring attribute .noreturn. because it conflicts with attribute .returns_twice." } */


int ATTR ((returns_twice, noreturn))
freturns_twice_noret2 (int);  /* { dg-warning "ignoring attribute .noreturn. because it conflicts with attribute .returns_twice." } */

int ATTR ((returns_twice)) ATTR ((noreturn))
freturns_twice_noret3 (int);  /* { dg-warning "ignoring attribute .noreturn. because it conflicts with attribute .returns_twice." } */


/* Exercise the interaction of multiple combinations of mutually
   exclusive attributes specified on distinct declarations.  */

inline int ATTR ((always_inline))
finline_cold_noreturn (int);

inline int ATTR ((cold))
finline_cold_noreturn (int);

inline int ATTR ((noreturn))
finline_cold_noreturn (int);

inline int ATTR ((noinline))
finline_cold_noreturn (int);    /* { dg-warning "ignoring attribute .noinline. because it conflicts with attribute .always_inline." } */

inline int ATTR ((hot))
finline_cold_noreturn (int);    /* { dg-warning "ignoring attribute .hot. because it conflicts with attribute .cold." } */

inline int ATTR ((warn_unused_result))
finline_cold_noreturn (int);    /* { dg-warning "ignoring attribute .warn_unused_result. because it conflicts with attribute .noreturn." } */

inline int ATTR ((always_inline))
finline_cold_noreturn (int);

/* Expect no warning for the missing return statement below because
   the function is noreturn.  */
inline int ATTR ((noreturn))
finline_cold_noreturn (int i) { (void)&i; __builtin_abort (); }


/* Exercise the interaction of multiple combinations of mutually
   exclusive attributes with some specified on the same declaration
   and some on distinct declarations.  */

inline int ATTR ((always_inline, hot))
finline_hot_noret_align (int);

inline int ATTR ((noreturn, noinline))
finline_hot_noret_align (int);  /* { dg-warning "ignoring attribute .noinline. because it conflicts with attribute .always_inline." } */

inline int ATTR ((cold, aligned (8)))
finline_hot_noret_align (int);  /* { dg-warning "ignoring attribute .cold. because it conflicts with attribute .hot." } */

inline int ATTR ((warn_unused_result))
finline_hot_noret_align (int);  /* { dg-warning "ignoring attribute .warn_unused_result. because it conflicts with attribute .noreturn." } */

inline int ATTR ((aligned (4)))
  finline_hot_noret_align (int);  /* { dg-warning "ignoring attribute .aligned \\(4\\). because it conflicts with attribute .aligned \\(8\\)." "" { target { ! { hppa*64*-*-* s390*-*-* } } } } */
/* { dg-error "alignment for '.*finline_hot_noret_align.*' must be at least 8" "" { target s390*-*-* } .-1 } */

inline int ATTR ((aligned (8)))
finline_hot_noret_align (int);

inline int ATTR ((const))
finline_hot_noret_align (int);  /* { dg-warning "ignoring attribute .const. because it conflicts with attribute .noreturn." } */

/* Expect no warning for the missing return statement below because
   the function is noreturn.  */
inline int ATTR ((noreturn))
finline_hot_noret_align (int i) { (void)&i; __builtin_abort (); }


/* Exercise variable attributes.  */

extern int ATTR ((common))
decl_common1;                 /* { dg-message "previous declaration here" } */

extern int ATTR ((nocommon))
decl_common1;                 /* { dg-warning "ignoring attribute .nocommon. because it conflicts with attribute .common." } */


extern int ATTR ((nocommon))
decl_common2;                 /* { dg-message "previous declaration here" } */

extern int ATTR ((common))
decl_common2;                 /* { dg-warning "ignoring attribute .common. because it conflicts with attribute .nocommon." } */


extern int ATTR ((common, nocommon))
decl_common3;                 /* { dg-warning "ignoring attribute .nocommon. because it conflicts with attribute .common." } */


extern int ATTR ((common, nocommon))
decl_common4;                 /* { dg-warning "ignoring attribute .nocommon. because it conflicts with attribute .common." } */
