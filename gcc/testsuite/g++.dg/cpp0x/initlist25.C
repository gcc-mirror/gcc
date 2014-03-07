// PR c++/41754
// { dg-do run { target c++11 } }

#include <map>
#include <string>
#include <iostream>

using namespace std;

int main()
{
        map<string, string> m;
        m.insert({{"t", "t"}, {"y", "y"}});

        return 0;
}
