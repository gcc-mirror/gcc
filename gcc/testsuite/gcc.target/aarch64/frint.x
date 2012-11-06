extern GPF SUFFIX(trunc) (GPF);
extern GPF SUFFIX(ceil) (GPF);
extern GPF SUFFIX(floor) (GPF);
extern GPF SUFFIX(nearbyint) (GPF);
extern GPF SUFFIX(rint) (GPF);
extern GPF SUFFIX(round) (GPF);

GPF test1a (GPF x)
{
  return SUFFIX(__builtin_trunc)(x);
}

GPF test1b (GPF x)
{
  return SUFFIX(trunc)(x);
}

GPF test2a (GPF x)
{
  return SUFFIX(__builtin_ceil)(x);
}

GPF test2b (GPF x)
{
  return SUFFIX(ceil)(x);
}

GPF test3a (GPF x)
{
  return SUFFIX(__builtin_floor)(x);
}

GPF test3b (GPF x)
{
  return SUFFIX(floor)(x);
}

GPF test4a (GPF x)
{
  return SUFFIX(__builtin_nearbyint)(x);
}

GPF test4b (GPF x)
{
  return SUFFIX(nearbyint)(x);
}

GPF test5a (GPF x)
{
  return SUFFIX(__builtin_rint)(x);
}

GPF test5b (GPF x)
{
  return SUFFIX(rint)(x);
}

GPF test6a (GPF x)
{
  return SUFFIX(__builtin_round)(x);
}

GPF test6b (GPF x)
{
  return SUFFIX(round)(x);
}
