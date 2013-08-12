// { dg-do compile }

// Origin: Volker Reichelt <reichelt@gcc.gnu.org>

// PR c++/18652: ICE redeclaring variable as template.

int A;			// { dg-message "previous declaration" }
template<int> struct A; // { dg-error "different kind of symbol" }
