/* Verify -Wmismatched-tags on alias definitions.
   { dg-do compile { target c++11 } }
   { dg-options "-Wall -Wredundant-tags" } */

class A;
using AA =        A;
using AA = class  A;          // { dg-warning "-Wredundant-tags" }
using AA = struct A;          // { dg-warning "-Wredundant-tags" }


template <class> class B;

using Bi =        B<int>;
using Bi = class  B<int>;     // { dg-warning "-Wredundant-tags" }
using Bi = struct B<int>;     // { dg-warning "-Wredundant-tags" }


template <class> class C;

template <class T>
using Cp = C<T*>;
template <class T>
using Cp = class C<T*>;       // { dg-warning "-Wredundant-tags" }
template <class T>
using Cp = struct C<T*>;      // { dg-warning "-Wredundant-tags" }
