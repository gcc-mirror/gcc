// PR c++/16337
// On i686-pc-linux-gnu, without options, we get:
//   warning: SSE vector return without SSE enabled changes the ABI
// { dg-options "-w" }

#define vector __attribute__((vector_size(16)))
vector signed int foo (void)
{
  vector signed int v1 = { 1, 2, 3, 4 };
  vector signed int v2, v3;
  vector signed int v4;
  v2 = v1;
  v3 = v1;
  v4 = v1;
}
