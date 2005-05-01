// { dg-options "-fno-weak" }

#if __GXX_WEAK__
#error "__GXX_WEAK__ defined when -fno-weak in use"
#endif
