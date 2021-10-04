// { dg-do compile }
// { dg-options "-O2 -w" }

int _bdf_parse_glyphs_bp;
long _bdf_parse_glyphs_nibbles;

void _bdf_parse_glyphs_p() 
{
  long p_2;

  _bdf_parse_glyphs_nibbles = p_2 << 1;

  for (; 0 < _bdf_parse_glyphs_nibbles;)
    if (1 < _bdf_parse_glyphs_nibbles)
      _bdf_parse_glyphs_bp = _bdf_parse_glyphs_p;
}
