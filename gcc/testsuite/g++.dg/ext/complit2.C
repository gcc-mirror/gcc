// PR c++/12726
// { dg-options "" }

#include <string>
 
struct foobar {
  std::string s;
};
 
int main(int argc, char **argv)
{
  foobar fb;

  fb = (foobar) { "abcd" };

  return 0;
}
