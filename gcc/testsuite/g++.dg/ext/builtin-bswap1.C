// PR c/37743
// { dg-do compile }

#if defined(__UINT32_TYPE__) && defined(__INT32_TYPE__)

void foo (__UINT32_TYPE__);
void foo (__INT32_TYPE__);

void
bar (__UINT32_TYPE__ x)
{
  foo (__builtin_bswap32 (x));
}

#else

void
bar ()
{
}

#endif
