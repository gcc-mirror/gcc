// DR 1423, PR c++/52174
// { dg-do compile { target c++11 } }

bool b1 = nullptr;  // { dg-error "direct" }

bool b2(nullptr);
bool b3{nullptr};

int  i1 = nullptr;  // { dg-error "cannot convert" }
int  i2(nullptr);   // { dg-error "cannot convert" }
int  i3{nullptr};   // { dg-error "cannot convert" }
