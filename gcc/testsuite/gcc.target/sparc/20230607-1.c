/* PR target.109541 */
/* Reported by Sam James <sjames@gcc.gnu.org> */

/* { dg-do compile } */
/* { dg-options "-O1 -mcpu=niagara4 -fpic -w" } */

int rhash_sha512_process_block_A, rhash_sha512_process_block_i,
rhash_sha512_process_block_block, rhash_sha512_process_block_W_0;

unsigned rhash_sha512_process_block_W_2;

void rhash_sha512_process_block (void)
{
  unsigned C, E, F, G, H, W_0, W_4, W_9, W_5, W_3, T1;

  for (; rhash_sha512_process_block_i; rhash_sha512_process_block_i += 6) {
    T1 = F + (rhash_sha512_process_block_W_2 += 6);
    rhash_sha512_process_block_A += H & G + (W_5 += rhash_sha512_process_block_W_0);
    H = C & T1 & E ^ F + (W_9 += rhash_sha512_process_block_W_0);
    G = T1 ^ 6 + (W_0 += rhash_sha512_process_block_block);
    F = (unsigned) &G;
    T1 = (unsigned) (&T1 + (W_3 += rhash_sha512_process_block_block > 9 > W_4));
    C = (unsigned) (T1 + &E);
    W_4 += W_5 += rhash_sha512_process_block_W_0;
  }
}
