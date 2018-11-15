int
_vfprintf_r (double fp)
{
  if (__builtin_signbit (fp))
    return '-';
}
