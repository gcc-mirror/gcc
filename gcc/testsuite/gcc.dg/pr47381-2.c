/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef unsigned long ulg;
long block_start;
typedef unsigned char uch;
extern uch window[];
 unsigned strstart;
ulg flush_block (char *buf, ulg stored_len, int eof);
ulg deflate()
{
    return flush_block(block_start >= 0L ? (char*)&window[(unsigned)block_start] : (char*)((void *)0), (long)strstart - block_start, (1));
}
