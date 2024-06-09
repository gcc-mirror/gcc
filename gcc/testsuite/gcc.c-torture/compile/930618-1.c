/* { dg-additional-options "-std=gnu89" } */

f (s)
{
  int r;

  r = (!g(s)) | (!g(s)) | (!g(s)) | (!g(s)) | (!g(s)) | (!g(s)) | (!g(s)) | (!g(s)) | (!g(s)) | (!g(s)) | (!g(s)) | (!g(s)) | (!g(s)) | (!g(s)) | (!g(s)) | (!g(s)) | (!g(s)) | (!g(s)) | (!g(s));

 return r;
}
