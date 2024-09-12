// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
// GROUPS passed old-abort
#include<iostream>

const int keys = 10;
const int key[keys] = {6, key[1], 2, keys, 1, 7, 6, key[2], key[8]};

void main()  // { dg-error "must return .int" }
{
        for(int i = 0; i < keys;) std::cout << key[i++] << " ";
        std::endl(std::cout);
}
