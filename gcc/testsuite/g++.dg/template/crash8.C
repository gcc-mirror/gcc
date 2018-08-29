// { dg-do compile }

// Origin: David Robinson <drtr@dial.pipex.com>

// PR c++/11513: ICE due to incorrect decision whether the tag is template.

template <typename T>
struct bar
{ 
  struct foo
  {
    int a;
  };

  template <typename U>
  void wom(U c)
  {
    struct foo b;
  }
};
