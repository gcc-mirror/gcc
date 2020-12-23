// { dg-additional-options {-fmodules-ts -Wno-psabi} }

import real2reel;

int main ()
{
  if (assassing () != 2.0f)
    return 1;

  if (market (/*square=*/2.0f, /*heroes=*/7.0) != 56.0)
    return 2;

  auto c_i = cinderella_search ();
  if (__real__ (c_i) != 1 || __imag__ (c_i) != 2)
    return 3;

  auto c_f = emerald_lies ();
  if (__real__ (c_f) != 3.0f || __imag__ (c_f) != 4.0f)
    return 4;

  auto c_d = forgotten_sons ();
  if (__real__ (c_d) != 5.0 || __imag__ (c_d) != 6.0)
    return 5;


  if (garden_party (7) != ' ')
    return 6;

  auto v = incubus ();
  if (v[0] != 1 || v[1] != 7 || v[2] != 3 || v[3] != 9)
    return 7;

  v = charting_the_single ();
  if (v[0] != 1 || v[1] != 2 || v[2] != 3 || v[3] != 4)
    return 8;

  return 0;
}

