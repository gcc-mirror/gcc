// { dg-do compile }

// Origin: Volker Lukas <vlukas@gmx.de>

// PR c++/9554: Access checking for template ID as class head.

class enclose
{
  template<typename T> struct enclosed;
};

template <>
struct enclose::enclosed<int>;
