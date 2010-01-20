// PR middle-end/42803
// { dg-do compile }
// { dg-options "-O0" }

#define X2 (a + a)
#define X4 (X2 + X2)
#define X8 (X4 + X4)
#define X16 (X8 + X8)
#define X32 (X16 + X16)
#define X64 (X32 + X32)
#define X128 (X64 + X64)
#define X256 (X128 + X128)
#define X512 (X256 + X256)
#define X1024 (X512 + X512)
#define X2048 (X1024 + X1024)
#define X4096 (X2048 + X2048)
#define X8192 (X4096 + X4096)
#define X16384 (X8192 + X8192)
#define X32768 (X16384 + X16384)
#define X65536 (X32768 + X32768)
#define X131072 (X65536 + X65536)
#define X262144 (X131072 + X131072)

int
foo (int a)
{
  int v = X262144;
  return v;
}

// Emit an error to just make sure we don't waste too much time
// in the middle-end compiling this.
int
bar (void)
{
  return x;	// { dg-error "was not declared in this scope" }
}
