/* PR middle-end/127984 */
/* { dg-options "-O2" } */
/* { dg-do run } */

__attribute__((noipa)) unsigned f(unsigned crc) {
  for (int i = 0; i < 24; i++) {
    if (crc & 1) crc = (crc >> 1) ^ 0xEDB88320; else crc >>= 1;
  }
  return crc;
}

int main(void) {
  if (f(0xFFFFFFFF) != 0xBE26ED)
    __builtin_abort();
}
