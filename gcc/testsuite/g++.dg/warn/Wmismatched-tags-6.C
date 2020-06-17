/* Verify -Wmismatched-tags on alias definitions.
   { dg-do compile { target c++11 } }
   { dg-options "-Wall -Wmismatched-tags" } */

class A;                      // { dg-message "declared as 'class'" }
using AA =        A;
using AA = class  A;
using AA = struct A;          // { dg-warning "-Wmismatched-tags" }


template <class> class B;     // { dg-message "declared as 'class'" }

using Bi =        B<int>;
using Bi = class  B<int>;
using Bi = struct B<int>;     // { dg-warning "-Wmismatched-tags" }
using Bi = class  B<int>;
using Bi = struct B<int>;     // { dg-warning "-Wmismatched-tags" }


template <class> class C;     // { dg-message "declared as 'class'" }

template <class T> using Cp = C<T*>;
template <class T> using Cp = class C<T*>;
template <class T>
using Cp = struct C<T*>;      // { dg-warning "-Wmismatched-tags" }

template <class T> using Cp = class  C<T*>;
template <class T>
using Cp = struct C<T*>;      // { dg-warning "-Wmismatched-tags" }
