// { dg-options "-Wreturn-type" }
// PR c++/20624

struct fpos {
 fpos(int __pos) {}
};
struct g {
  g();
  ~g();
};
fpos seekoff(int b, int c)
{
  g __buf;
  if (b != -1 && c >= 0)
    return fpos(-1);
  else
    return fpos(-1);
} 
