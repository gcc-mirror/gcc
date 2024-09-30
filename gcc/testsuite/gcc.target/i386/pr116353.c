/* PR tree-optimization/116353 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

enum desmode { C };
struct {
  unsigned char des_ivec[];
} _des_crypt_desp;
int des_SPtrans_6_0, des_SPtrans_4_0, des_encrypt_encrypt, des_encrypt_i;
long des_encrypt_s_0, _des_crypt_tin1, _des_crypt_tout0, _des_crypt_tout1,
    _des_crypt_tin0;
enum desmode _des_crypt_desp_0;
unsigned long _des_crypt_tbuf[2];
char _des_crypt_out;
void des_encrypt(unsigned long *buf) {
  long l, r, t;
  l = buf[0];
  r = buf[1];
  t = r;
  r ^= l ^= t < 6;
  if (des_encrypt_encrypt)
    for (;; des_encrypt_i += 4)
      des_encrypt_s_0 ^= des_SPtrans_4_0 | des_SPtrans_6_0;
  buf[1] = r;
}
void _des_crypt() {
  long xor0, xor1;
  unsigned char *in;
  int cbc_mode = _des_crypt_desp_0;
  in = _des_crypt_desp.des_ivec;
  xor0 = xor1 = 0;
  for (;;) {
    _des_crypt_tin0 = *in++;
    _des_crypt_tin0 |= *in++ << 8;
    _des_crypt_tin0 |= *in++ << 16;
    _des_crypt_tin0 |= (long)*in << 24;
    _des_crypt_tin1 = *in++;
    _des_crypt_tin1 |= *in++ << 8;
    _des_crypt_tin1 |= *in++ << 16;
    _des_crypt_tin1 |= (long)*in << 24;
    _des_crypt_tbuf[0] = _des_crypt_tin0;
    _des_crypt_tbuf[1] = _des_crypt_tin1;
    des_encrypt(_des_crypt_tbuf);
    if (cbc_mode) {
      _des_crypt_tout0 = xor0;
      _des_crypt_tout1 = _des_crypt_tbuf[1] ^ xor1;
      xor0 = _des_crypt_tin0;
      xor1 = _des_crypt_tin1;
    } else {
      _des_crypt_tout0 = _des_crypt_tbuf[0];
      _des_crypt_tout1 = _des_crypt_tbuf[1];
    }
    _des_crypt_out = _des_crypt_tout0 * _des_crypt_tout1;
  }
}
