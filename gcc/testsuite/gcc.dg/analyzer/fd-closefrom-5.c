/* PR analyzer/106283 */
/* Verify closefrom does not ICE on a tracked non-int svalue_from_bit_vector.  */

extern int bind (long long fd, const void *addr, unsigned len);
extern void closefrom (int lowfd);

void
test_closefrom_non_int_bitvec (long long fd, const void *addr)
{
  fd |= 1;
  bind (fd, addr, 16);
  closefrom (3);
}
