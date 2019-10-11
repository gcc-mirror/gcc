// { dg-do compile { target c++2a } }

template<class T, class U>
concept Same = __is_same_as(T, U);

template<typename T>
concept C1 = true;

template<typename T, typename U>
concept C2 = true;

C1 auto        c1 = 0;
C2<int> auto   c2 = 0;
Same<int> auto s1 = 'a'; // { dg-error "does not satisfy|is_same" }
