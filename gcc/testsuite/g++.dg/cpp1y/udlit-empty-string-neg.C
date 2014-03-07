// { dg-do compile { target c++1y } }

int
operator "*"_s(unsigned long long) // { dg-error "expected empty string after 'operator'" }
{ return 0; }

int
operator L"*"_Ls(unsigned long long) // { dg-error "invalid encoding prefix in literal operator" }
{ return 0; }

int
operator u"*"_s16(unsigned long long) // { dg-error "invalid encoding prefix in literal operator" }
{ return 0; }

int
operator U"*"_s32(unsigned long long) // { dg-error "invalid encoding prefix in literal operator" }
{ return 0; }

int
operator u8"*"_u8s(unsigned long long) // { dg-error "invalid encoding prefix in literal operator" }
{ return 0; }
