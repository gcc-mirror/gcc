/* { dg-do compile } */
/* { dg-options "" } */

/* PR middle-end/120709 */
/* Make sure we don't ICE on a non-constant poly argument. */


typedef unsigned char uint8_t;
uint8_t crc8_data8(uint8_t crc, uint8_t data, uint8_t polynomial) {
  return __builtin_rev_crc32_data8 (crc, data, polynomial); /* { dg-error "must be a constant" } */
}
