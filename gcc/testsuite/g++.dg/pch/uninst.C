#include "uninst.H"

template <class Type> void FOO() {   }

int  main() {
  		FOO<char>(); 			// stage 2 needs this
		return min<unsigned long>(5, 0);
}
