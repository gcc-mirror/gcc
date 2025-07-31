// PR c++/120800
// { dg-do compile { target c++11 } }

template<typename T>
struct Container
{
 T m_data[1] {};
};

class Element
{
private:
 Element() = default;

private:
 bool m_bool1 { false };
 bool m_bool2;

 friend struct Container<Element>;
};

Container<Element> element;
