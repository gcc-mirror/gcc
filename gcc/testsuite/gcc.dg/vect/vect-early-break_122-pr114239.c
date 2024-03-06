/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */

int ip4_getbit_a, ip4_getbit_pos, ip4_clrbit_pos;
void ip4_clrbit(int *a) { *a &= ip4_clrbit_pos; }
typedef struct {
  char pxlen;
  int prefix;
} net_addr_ip4;
void fib_get_chain();
int trie_match_longest_ip4();
int trie_match_next_longest_ip4(net_addr_ip4 *n) {
  int __trans_tmp_1;
  while (n->pxlen) {
    n->pxlen--;
    ip4_clrbit(&n->prefix);
    __trans_tmp_1 = ip4_getbit_a >> ip4_getbit_pos;
    if (__trans_tmp_1)
      return 1;
  }
  return 0;
}
void net_roa_check_ip4_trie_tab() {
  net_addr_ip4 px0;
  for (int _n = trie_match_longest_ip4(&px0); _n;
       _n = trie_match_next_longest_ip4(&px0))
    fib_get_chain();
}
