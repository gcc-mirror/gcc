// { dg-do compile }
// { dg-options "-Wall" }

// Origin: stip@mathematik.uni-ulm.de
//	   Andrew Pinski <pinskia@gcc.gnu.org>

// PR c++/13106: No return warning when return type is a dependent type.

template <typename T> T dummy() { }

int main() { 
    dummy<void>(); 
}
