// { dg-do compile }

// Origin: Volker Reichelt <reichelt@gcc.gnu.org>

// PR c++/18123: ICE pushing tag from invalid template.

template<int> enum E { e }; // { dg-error "template declaration" }
