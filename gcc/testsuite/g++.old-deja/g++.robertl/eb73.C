// { dg-do run  }
// { dg-options "-O -Wall" }
// Depletes VM.

#include <iostream>
#include <list>
#include <algorithm>
using namespace std;

int main()
{
    int daten [16] = { 1, 4, 4, 6, 1, 2, 2, 3, 6, 6, 6, 5, 7, 5, 4, 4};
    list<int> menge;
    copy (daten, daten+16, back_inserter(menge));
}
