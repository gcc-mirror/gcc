// https://issues.dlang.org/show_bug.cgi?id=19941

/******************************************/

immutable i4 = 42;
const v4 = new C4;
class C4 { int f4 = i4; }

const v5 = new C5;
immutable i5 = 42;
class C5 { int f5 = i5; }

const v6 = new C6;
class C6 { int f6 = i6; }
immutable i6 = 42;

/******************************************/

immutable i10 = 42;
__gshared v10 = new C10;
class C10 { int f10 = i10; }

__gshared v11 = new C11;
immutable i11 = 42;
class C11 { int f11 = i11; }

__gshared v12 = new C12;
class C12 { int f12 = i12; }
immutable i12 = 42;

/******************************************/

immutable i13 = 42;
immutable v13 = new C13;
class C13 { int f13 = i13; }

immutable v14 = new C14;
immutable i14 = 42;
class C14 { int f14 = i14; }

immutable v15 = new C15;
class C15 { int f15 = i15; }
immutable i15 = 42;

/******************************************/

immutable i16 = 42;
shared v16 = new C16;
class C16 { int f16 = i16; }

shared v17 = new C17;
immutable i17 = 42;
class C17 { int f17 = i17; }

shared v18 = new C18;
class C18 { int f18 = i18; }
immutable i18 = 42;
