extern GPF SUFFIX(trunc) (GPF);
extern GPF SUFFIX(ceil) (GPF);
extern GPF SUFFIX(floor) (GPF);
extern GPF SUFFIX(round) (GPF);

GPI test1a (GPF x) {
  return SUFFIX(__builtin_trunc)(x);
}

GPI test1b (GPF x)
{
  return SUFFIX(trunc)(x);
}

GPI test2a (GPF x)
{
  return SUFFIX(__builtin_lceil)(x);
}

GPI test2b (GPF x)
{
  return SUFFIX(ceil)(x);
}

GPI test2c (GPF x)
{
  return SUFFIX(__builtin_ceil)(x);
}

GPI test3a (GPF x)
{
  return SUFFIX(__builtin_lfloor)(x);
}

GPI test3b (GPF x)
{
  return SUFFIX(floor)(x);
}

GPI test3c (GPF x)
{
  return SUFFIX(__builtin_floor)(x);
}

GPI test4a (GPF x)
{
  return SUFFIX(__builtin_round)(x);
}

GPI test4b (GPF x)
{
  return SUFFIX(round)(x);
}


