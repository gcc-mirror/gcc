/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef unsigned IPos;
typedef unsigned char uch;
extern uch window[];
unsigned max_chain_length;
unsigned strstart;
int longest_match(IPos cur_match, int len, int best_len)
{
    unsigned chain_length = max_chain_length;
    register uch *scan = window + strstart;
    register uch *match;
    register uch scan_end1 = scan[best_len-1];
    register uch scan_end = scan[best_len];
    do {
        ;
        match = window + cur_match;
        if (match[best_len] != scan_end ||
            match[best_len-1] != scan_end1 ||
            *match != *scan ||
            *++match != scan[1]) continue;
            best_len = len;
    } while ( --chain_length != 0);
    return best_len;
}
