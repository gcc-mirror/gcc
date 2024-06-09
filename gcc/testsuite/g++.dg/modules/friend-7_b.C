// { dg-additional-options "-fmodules-ts" }
#include "friend-7.h"
import "friend-7_a.H";

A<int> a;
A<int>::B<char> b;
