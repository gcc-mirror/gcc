char _hex_value[256];

void
hex_init ()
{
  int i;
  for (i = 0; i < 256; i++)
    _hex_value[i] = 99;
  for (i = 0; i < 10; i++)
    _hex_value['0' + i] = i;
}
