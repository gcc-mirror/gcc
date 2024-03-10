/* { dg-do run } */

struct libkeccak_generalised_spec {
  int state_size;
  int word_size;
} main_gspec;

long gvar;

int libkeccak_degeneralise_spec(struct libkeccak_generalised_spec *spec)
{
  int state_size;
  int have_state_size = spec->state_size != -1;
  int have_word_size = spec->word_size;

  if (have_state_size)
    state_size = spec->state_size;
  if (have_word_size)
    gvar = 12345;
  if (have_state_size && state_size != spec->word_size)
    return 1;
  if (spec)
    gvar++;
  return 0;
}

int main()
{
  main_gspec.state_size = -1;
  if (libkeccak_degeneralise_spec(&main_gspec))
    __builtin_abort();
  return 0;
}
