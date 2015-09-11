// { dg-do compile { target c++14 } }
// { dg-prune-output "note:" }

template<bool, typename _Tp = void>struct enable_if {};
template<typename _Tp> struct enable_if<true, _Tp> { typedef _Tp type; };


template<typename CharT, CharT... String>
typename enable_if<sizeof...(String) == 1, int>::type operator"" _script () { // { dg-error "no type named|in" }
  return 1;
}

template<typename CharT, CharT... String>
typename enable_if<sizeof...(String) == 2, int>::type operator"" _script () {  // { dg-error "no type named|in" }
  return 2;
}

int a = "1"_script;
int b = "22"_script;
int c = "333"_script; // { dg-error "no matching function for call to"}
