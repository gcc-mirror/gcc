// { dg-do preprocess }
// { dg-additional-options "-fmodules -M" }

inline void f() { }

// { dg-final { scan-file dep-4.i {dep-4\.H\.c\+\+-header-unit:} } }
// { dg-final { scan-file-not dep-4.i {inline} } }
