static const char OggFLAC__MAPPING_VERSION_MAJOR = 1;
void f(void)
{
  char synthetic_first_packet_body[10];
  char *b = &synthetic_first_packet_body[4];
  __builtin_memcpy (b, &OggFLAC__MAPPING_VERSION_MAJOR, (1u));
}


