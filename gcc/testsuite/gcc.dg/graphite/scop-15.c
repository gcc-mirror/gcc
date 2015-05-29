#  define EXTERN(type, array)  extern type array[]
typedef unsigned char  uch;
typedef unsigned short ush;
EXTERN(uch, window);
EXTERN(ush, prev);
#ifndef WSIZE
#  define WSIZE 0x8000
#endif                
#define MIN_MATCH  3
#define MAX_MATCH  258
#define MIN_LOOKAHEAD (MAX_MATCH+MIN_MATCH+1)
#define MAX_DIST  (WSIZE-MIN_LOOKAHEAD)
#define near
typedef unsigned IPos;
unsigned near max_chain_length;
extern unsigned near strstart;
unsigned int near prev_length;
#define NIL 0
unsigned near good_match;
int near nice_match;
#define WMASK     (WSIZE-1)
int longest_match(IPos cur_match)
{
    unsigned chain_length = max_chain_length;
    register uch *scan = window + strstart;  
    register uch *match;                     
    register int len;                        
    int best_len = prev_length;              
    IPos limit = strstart > (IPos)MAX_DIST ? strstart - (IPos)MAX_DIST : NIL;
    register uch *strend = window + strstart + MAX_MATCH;
    register uch scan_end   = scan[best_len];
    if (prev_length >= good_match) {
    }
    do {
        if (match[best_len]   != scan_end  ||
            *++match          != scan[1])      continue;
        do {
        } while (*++scan == *++match && *++scan == *++match &&
                 scan < strend);
        len = MAX_MATCH - (int)(strend - scan);
        if (len > best_len) {
            best_len = len;
            if (len >= nice_match) break;
        }
    } while ((cur_match = prev[cur_match & WMASK]) > limit
	     && --chain_length != 0);
    return best_len;
}

/* { dg-final { scan-tree-dump-times "number of SCoPs: 0" 1 "graphite"} } */
