/* PR middle-end/37730 */
/* { dg-do compile } */

void sdp_seq_alloc (void *);

void
add_opush (void)
{
  unsigned char formats[] = { 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xff };
  void *dtds[sizeof (formats)];
  unsigned int i;
  unsigned char dtd = 0x08;
  for (i = 0; i < sizeof (formats); i++)
    dtds[i] = &dtd;
  sdp_seq_alloc (dtds);
}

