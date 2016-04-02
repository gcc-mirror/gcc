// PR c++/65398
// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

constexpr char s[] = "abc";
constexpr char c1 = *(&s[0] + 0);
constexpr char c2 = *(&s[0] + 1);
constexpr char c3 = *(&s[1] + 0);
constexpr char c4 = *(&s[1] + 1);
constexpr char c5 = *(&s[2] + 0);
constexpr char c6 = *(&s[0] + 2);
constexpr char c7 = *(&s[2] + 1);

constexpr char d1 = *(&s[4] - 0); // { dg-error "array subscript" }
constexpr char d2 = *(&s[4] - 1);
constexpr char d3 = *(&s[4] - 2);
constexpr char d4 = *(&s[4] - 3);
constexpr char d5 = *(&s[4] - 4);
constexpr char d6 = *(&s[4] - 5);  // { dg-error "array subscript" }

/* Don't accept invalid stuff.  */
constexpr char e1 = *(&s[5] - 1); // { dg-error "is not a constant expression" }
constexpr char e2 = *(&s[5] - 2); // { dg-error "is not a constant expression" }
constexpr char e3 = *(&s[5] - 3); // { dg-error "is not a constant expression" }

SA (c1 == 'a');
SA (c2 == 'b');
SA (c3 == 'b');
SA (c4 == 'c');
SA (c5 == 'c');
SA (c6 == 'c');
SA (c7 == '\0');
SA (d2 == '\0');
SA (d3 == 'c');
SA (d4 == 'b');
SA (d5 == 'a');

constexpr int l[] = { 'c', 'd', 'e', '\0' };
constexpr int i1 = *(&l[0] + 0);
constexpr int i2 = *(&l[0] + 1);
constexpr int i3 = *(&l[1] + 0);
constexpr int i4 = *(&l[1] + 1);
constexpr int i5 = *(&l[2] + 0);
constexpr int i6 = *(&l[0] + 2);
constexpr int i7 = *(&l[2] + 1);

constexpr char j1 = *(&l[4] - 0); // { dg-error "array subscript" }
constexpr char j2 = *(&l[4] - 1);
constexpr char j3 = *(&l[4] - 2);
constexpr char j4 = *(&l[4] - 3);
constexpr char j5 = *(&l[4] - 4);
constexpr char j6 = *(&l[4] - 5);  // { dg-error "array subscript" }

/* Don't accept invalid stuff.  */
constexpr char k1 = *(&l[5] - 1); // { dg-error "is not a constant expression" }
constexpr char k2 = *(&l[5] - 2); // { dg-error "is not a constant expression" }
constexpr char k3 = *(&l[5] - 3); // { dg-error "is not a constant expression" }

SA (i1 == 'c');
SA (i2 == 'd');
SA (i3 == 'd');
SA (i4 == 'e');
SA (i5 == 'e');
SA (i6 == 'e');
SA (i7 == '\0');
SA (j2 == '\0');
SA (j3 == 'e');
SA (j4 == 'd');
SA (j5 == 'c');
