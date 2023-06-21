/* { dg-do run } */

struct libkeccak_spec {
    long int bitrate;
};

struct libkeccak_generalised_spec {
    long int bitrate;
    long int state_size;
    long int word_size;
};

int __attribute__((noipa))
libkeccak_degeneralise_spec(struct libkeccak_generalised_spec *restrict spec,
			    struct libkeccak_spec *restrict output_spec)
{
  long int state_size, word_size, bitrate, output;
  const int have_state_size = spec->state_size != (-65536L);
  const int have_word_size = spec->word_size != (-65536L);
  const int have_bitrate = spec->bitrate != (-65536L);

  if (have_state_size)
    {
      state_size = spec->state_size;
      if (state_size <= 0)
	return 1;
      if (state_size > 1600)
	return 2;
    }

  if (have_word_size)
    {
      word_size = spec->word_size;
      if (word_size <= 0)
	return 4;
      if (word_size > 64)
	return 5;
      if (have_state_size && state_size != word_size * 25)
	return 6;
      else if (!have_state_size) {
	  spec->state_size = 1;
	  state_size = word_size * 25;
      }
    }

  if (have_bitrate)
    bitrate = spec->bitrate;

  if (!have_bitrate)
    {
      state_size = (have_state_size ? state_size : (1600L));
      output = ((state_size << 5) / 100L + 7L) & ~0x07L;
      bitrate = output << 1;
    }

  output_spec->bitrate = bitrate;

  return 0;
}

int main ()
{
  struct libkeccak_generalised_spec gspec;
  struct libkeccak_spec spec;
  spec.bitrate = -1;
  gspec.bitrate = -65536;
  gspec.state_size = -65536;
  gspec.word_size = -65536;
  if (libkeccak_degeneralise_spec(&gspec, &spec))
    __builtin_abort ();
  if (spec.bitrate != 1024)
    __builtin_abort ();
  return 0;
}
