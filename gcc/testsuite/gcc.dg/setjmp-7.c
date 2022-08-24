/* { dg-do compile } */
/* { dg-options "-O2 -fno-guess-branch-probability -w" } */
/* { dg-require-effective-target indirect_jumps } */

struct __jmp_buf_tag { };
typedef struct __jmp_buf_tag jmp_buf[1];
struct globals { jmp_buf listingbuf; };
extern struct globals *const ptr_to_globals;
void foo()
{
    if ( _setjmp ( ((*ptr_to_globals).listingbuf )))
	;
}
