/* Test function like macro. */
/* Contributed by Devang Patel <dpatel@apple.com> */

/* {do-do preprocess } */
/* { dg-options "-traditional-cpp -E -dD" } */
int     __srget (char *);
#define __sgetc(p) (--(p)->_r < 0 ? __srget(p) : (int)(*(p)->_p++))
#define getc(fp)        __sgetc(fp)
#define getchar()       getc(stdin)

