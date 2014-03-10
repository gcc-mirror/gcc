// { dg-do compile { target c++11 } }

// PR c++/58072

extern 'c'void*blah(void*); // { dg-error "expected unqualified-id before user-defined character literal" }
extern L'c'void*Lblah(void*); // { dg-error "expected unqualified-id before user-defined character literal" }
extern u'c'void*ublah(void*); // { dg-error "expected unqualified-id before user-defined character literal" }
extern U'c'void*Ublah(void*); // { dg-error "expected unqualified-id before user-defined character literal" }

extern "c"void*strblah(void*); // { dg-error "expected unqualified-id before user-defined string literal" }
extern L"c"void*Lstrblah(void*); // { dg-error "expected unqualified-id before user-defined string literal" }
extern u"c"void*ustrblah(void*); // { dg-error "expected unqualified-id before user-defined string literal" }
extern U"c"void*Ustrblah(void*); // { dg-error "expected unqualified-id before user-defined string literal" }
extern u8"c"void*u8strblah(void*); // { dg-error "expected unqualified-id before user-defined string literal" }

extern 123void*ULLblah(void*); // { dg-error "expected unqualified-id before numeric constant" }
extern 123.456void*Ldblblah(void*); // { dg-error "expected unqualified-id before numeric constant" }
