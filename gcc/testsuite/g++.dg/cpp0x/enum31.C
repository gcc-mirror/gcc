// { dg-do compile { target c++11 } }

enum class __attribute__((__visibility__("default"))) Foobar
{
  fratz,
    nabble
};
