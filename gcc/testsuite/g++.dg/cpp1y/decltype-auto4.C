// PR c++/103403
// { dg-do compile { target c++14 } }

struct false_type { static constexpr bool value = false; };
struct true_type { static constexpr bool value = true; };
template<class T, class U>
struct is_same : false_type {}; 
template<class T>
struct is_same<T, T> : true_type {};

int fn ();
int &ref ();
int &&rref ();

struct S {
  int i;
  int &r = i;
};

void
ids ()
{
  const S *s = new S();
  int i;
  int &ir = i;
  decltype(auto) r1 = s->i;
  static_assert (is_same<decltype(r1), int>::value, "");
  decltype(auto) r2 = s->r;
  static_assert (is_same<decltype(r2), int&>::value, "");
  decltype(auto) r3 = i;
  static_assert (is_same<decltype(r3), int>::value, "");
  decltype(auto) r4 = ir;
  static_assert (is_same<decltype(r4), int&>::value, "");
}

void
nonids ()
{
  const S *s = new S();
  int i;
  int &ir = i;
  int &&irr = 42;
  decltype(auto) r1 = fn ();
  static_assert (is_same<decltype(r1), int>::value, ""); 
  decltype(auto) r2 = (fn ());
  static_assert (is_same<decltype(r2), int>::value, ""); 
  decltype(auto) r3 = ref ();
  static_assert (is_same<decltype(r3), int&>::value, ""); 
  decltype(auto) r4 = (ref ());
  static_assert (is_same<decltype(r4), int&>::value, ""); 
  decltype(auto) r5 = rref ();
  static_assert (is_same<decltype(r5), int&&>::value, ""); 
  decltype(auto) r6 = (rref ());
  static_assert (is_same<decltype(r6), int&&>::value, ""); 
  decltype(auto) r8 = (s->i);
  static_assert (is_same<decltype(r8), const int&>::value, "");
  decltype(auto) r9 = (s->r);
  static_assert (is_same<decltype(r9), int&>::value, "");
  decltype(auto) r10 = (i);
  static_assert (is_same<decltype(r10), int&>::value, "");
  decltype(auto) r11 = (ir);
  static_assert (is_same<decltype(r11), int&>::value, "");
  decltype(auto) r12 = (irr);
  static_assert (is_same<decltype(r12), int&>::value, "");
}
