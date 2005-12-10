/* PR opt/13862 */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O" } */

typedef struct _fame_syntax_t_ {
} fame_syntax_t;

typedef struct _fame_bitbuffer_t_
{
  unsigned char * base;
  unsigned char * data;
  unsigned long shift;
} fame_bitbuffer_t;

#define fast_bitbuffer_write(data, shift, c, l)				\
{									\
  int d;								\
									\
  asm("add %1, %%ecx\n"            /* ecx = shift + length */		\
      "shrd %%cl, %2, %3\n"        /* adjust code to fit in */		\
      "shr %%cl, %2\n"             /* adjust code to fit in */		\
      "mov %%ecx, %1\n"            /* shift += length */		\
      "bswap %2\n"                 /* reverse byte order of code */	\
      "shr $5, %%ecx\n"            /* get dword increment */            \
      "or %2, (%0)\n"              /* put first 32 bits */		\
      "bswap %3\n"                 /* reverse byte order of code */	\
      "lea   (%0, %%ecx, 4), %0\n" /* data += (ecx>32) */		\
      "andl $31, %1\n"             /* mask shift */			\
      "orl %3, (%0)\n"             /* put last 32 bits */		\
      : "=r"(data), "=r"(shift), "=a"(d), "=d"(d), "=c"(d)		\
      : "0"(data), "1"(shift), "2"((unsigned long) c), "3"(0),		\
	"c"((unsigned long) l)						\
      : "memory");							\
}

#define bitbuffer_write(bb, c, l) \
  fast_bitbuffer_write((bb)->data, (bb)->shift, c, l)

typedef enum { frame_type_I, frame_type_P } frame_type_t;

typedef struct _fame_syntax_mpeg1_t_ {
  fame_bitbuffer_t buffer;
  frame_type_t frame_type;
} fame_syntax_mpeg1_t;

#define FAME_SYNTAX_MPEG1(x) ((fame_syntax_mpeg1_t *) x)

void mpeg1_start_picture(fame_syntax_t *syntax)
{
  fame_syntax_mpeg1_t *syntax_mpeg1 = FAME_SYNTAX_MPEG1(syntax);
  bitbuffer_write(&syntax_mpeg1->buffer, 0xFFFF, 16); 

  switch(syntax_mpeg1->frame_type) {
    case frame_type_I:
      bitbuffer_write(&syntax_mpeg1->buffer, 0, 1);  
    break;
    case frame_type_P:
      bitbuffer_write(&syntax_mpeg1->buffer, 0, 1);
    break;
  }
}
