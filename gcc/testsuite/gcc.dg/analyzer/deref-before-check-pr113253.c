/* Regression test for PR analyzer/113253 which was showing analyzer
   differences with and without -g.

   C only: reduced reproducer doesn't easily work with C++.  */

/* { dg-additional-options "-O2 -g" } */

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __UINTPTR_TYPE__ uintptr_t;
typedef __PTRDIFF_TYPE__ EMACS_INT;
enum
{
  EMACS_INT_WIDTH = sizeof(EMACS_INT) * 8,
  VALBITS = EMACS_INT_WIDTH - 3,
};
typedef struct Lisp_X* Lisp_Word;
enum Lisp_Type
{
  Lisp_Symbol = 0,
  Lisp_Vectorlike = 5,
};
typedef Lisp_Word Lisp_Object;
static inline EMACS_INT(XLI)(Lisp_Object o)
{
  return ((EMACS_INT)(o));
}
static inline void*(XLP)(Lisp_Object o)
{
  return ((void*)(o));
}
struct Lisp_Symbol
{};
typedef uintptr_t Lisp_Word_tag;
extern struct Lisp_Symbol lispsym[1608];
union vectorlike_header
{
  ptrdiff_t size;
};
enum pvec_type
{
  PVEC_MARKER,
};
enum More_Lisp_Bits
{
  PSEUDOVECTOR_SIZE_BITS = 12,
  PSEUDOVECTOR_REST_BITS = 12,
  PSEUDOVECTOR_AREA_BITS = PSEUDOVECTOR_SIZE_BITS + PSEUDOVECTOR_REST_BITS,
  PVEC_TYPE_MASK = 0x3f << PSEUDOVECTOR_AREA_BITS
};
static inline _Bool
PSEUDOVECTORP(Lisp_Object a, int code)
{
  return (
    ((((union vectorlike_header*)((uintptr_t)XLP((a)) -
                                  (uintptr_t)(
                                    (Lisp_Word_tag)(Lisp_Vectorlike)
                                    << (((0x7fffffffffffffffL >> (3 - 1)) / 2 <
                                         (9223372036854775807L))
                                          ? 0
                                          : VALBITS))))
        ->size &
      (((9223372036854775807L) - (9223372036854775807L) / 2) |
       PVEC_TYPE_MASK)) ==
     (((9223372036854775807L) - (9223372036854775807L) / 2) |
      ((code) << PSEUDOVECTOR_AREA_BITS))));
}
static inline Lisp_Object
make_lisp_symbol(struct Lisp_Symbol* sym)
{
  Lisp_Object a = ((Lisp_Word)(
    ((Lisp_Word_tag)(Lisp_Symbol)
     << (((0x7fffffffffffffffL >> (3 - 1)) / 2 < (9223372036854775807L))
           ? 0
           : VALBITS))));
  return a;
}
static inline Lisp_Object
builtin_lisp_symbol(int index)
{
  return make_lisp_symbol(&lispsym[index]);
}
static inline _Bool(BASE_EQ)(Lisp_Object x, Lisp_Object y)
{
  return (XLI(x) == XLI(y));
}
static inline _Bool(NILP)(Lisp_Object x)
{
  return BASE_EQ(x, builtin_lisp_symbol(0));
}
struct thread_state
{
  struct buffer* m_current_buffer;
};
extern struct thread_state* current_thread;
struct Lisp_Marker
{
  struct buffer* buffer;
};
static inline _Bool
MARKERP(Lisp_Object x)
{
  return PSEUDOVECTORP(x, PVEC_MARKER);
}
static inline struct Lisp_Marker*
XMARKER(Lisp_Object a)
{
  return ((
    struct Lisp_Marker*)((uintptr_t)XLP(a) -
                         (uintptr_t)((Lisp_Word_tag)(Lisp_Vectorlike)
                                     << (((0x7fffffffffffffffL >> (3 - 1)) / 2 <
                                          (9223372036854775807L))
                                           ? 0
                                           : VALBITS))));
}
extern void
unchain_marker();
struct buffer
{
  Lisp_Object name_;
};
static inline struct buffer*
XBUFFER(Lisp_Object a)
{
  return (
    (struct buffer*)((uintptr_t)XLP(a) -
                     (uintptr_t)((Lisp_Word_tag)(Lisp_Vectorlike)
                                 << (((0x7fffffffffffffffL >> (3 - 1)) / 2 <
                                      (9223372036854775807L))
                                       ? 0
                                       : VALBITS))));
}
static inline _Bool
BUFFER_LIVE_P(struct buffer* b)
{
  return !NILP(((b)->name_));
}
static inline struct buffer*
decode_buffer(Lisp_Object b)
{
  return NILP(b) ? (current_thread->m_current_buffer) : (XBUFFER(b));
}
static struct buffer*
live_buffer(Lisp_Object buffer)
{
  struct buffer* b = decode_buffer(buffer);
  return BUFFER_LIVE_P(b) ? b : ((void*)0);
}
Lisp_Object
set_marker_internal(Lisp_Object position, Lisp_Object buffer)
{
  struct buffer* b = live_buffer(buffer);
  if (NILP(position) || (MARKERP(position) && !XMARKER(position)->buffer) || !b) /* { dg-bogus "Wanalyzer-deref-before-check" } */
    unchain_marker();
  return 0;
}
