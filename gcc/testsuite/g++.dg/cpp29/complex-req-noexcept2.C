// C++29 P3822R2 - Conditional noexcept specifiers in compound requirements
// { dg-do compile { target c++20 } }
// { dg-options "" }

template <typename T, int N>
concept A = sizeof (T) < N;
template <typename T>
concept B = A <T, 1000>;
template <typename T>
struct C { using type = T; };

template <typename T>
void foo (int n)
requires requires {
  T ();
  n;
  n == T ();
  { T () + 1 } -> B;
  { T () - 1 } noexcept;
  { T () * 1 } noexcept -> A <1234>;
  { T () + 2 } noexcept (true);				// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
  { T () - 2 } noexcept (true) -> A <1234>;		// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
  { T () * 2 } noexcept (false);			// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
  { T () + 3 } noexcept (false) -> A <1234>;		// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
  { T () - 3 } noexcept (sizeof (T) > 100);		// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
  { T () * 3 } noexcept (sizeof (T) > 100) -> A <1234>;	// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
  typename T;
  typename C <T>;
  typename C <T>::type;
  typename C <decltype (n)>;
  requires A <T, 256>;
}
{
}

template
void foo <int> (int);

// { dg-final { scan-assembler "_Z3fooIiEviQrqXcvT__EXfL0p_XeqfL0p_cvS0__EXplcvS0__ELi1ER1BXmicvS0__ELi1ENXmlcvS0__ELi1ENR1AILi1234EEXplcvS0__ELi2ENXmicvS0__ELi2ENR1AILi1234EEXmlcvS0__ELi2EXplcvS0__ELi3ER1AILi1234EEXmicvS0__ELi3ECgtstS0_Li100EXmlcvS0__ELi3ECgtstS0_Li100ER1AILi1234EETS0_T1CIS0_ETNS2_4typeETS1_IiEQ1AIS0_Li256EEE" } }
