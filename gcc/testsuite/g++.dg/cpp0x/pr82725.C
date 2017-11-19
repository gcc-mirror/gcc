// { dg-do compile { target { { i?86-*-* x86_64-*-* } && c++11 } } }
// { dg-require-effective-target pie }
// { dg-options "-O2 -fpie -mtls-direct-seg-refs" }

struct string
{
  __SIZE_TYPE__ length;
  const char *ptr;
};

string
tempDir ()
{
  thread_local string cache;
  return cache;
}
