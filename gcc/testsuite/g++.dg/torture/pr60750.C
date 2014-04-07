// { dg-do run }
// { dg-options "-std=c++11" }

#include <string>
#include <stdexcept>

const std::string err_prefix = "Problem: ";
void thrower (std::string msg)
{
  throw std::runtime_error(err_prefix + std::move(msg));
}

int main(int argc, char **argv)
{
  try {
      std::string base = "hello";
      thrower(std::move(base));
  } catch (const std::runtime_error &e) {
  }
  return 0;
}
