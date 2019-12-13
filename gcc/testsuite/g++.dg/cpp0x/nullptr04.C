// { dg-do compile { target c++11 } }

// Test cast to int

__extension__ typedef __INTPTR_TYPE__ intptr_t;

const int n4 = static_cast<const int>(nullptr); // { dg-error "16:invalid 'static_cast' " }
const short int n5 = reinterpret_cast<short int>(nullptr); // { dg-error "22:cast from .std::nullptr_t. to .short int. loses precision" }
const intptr_t n6 = reinterpret_cast<intptr_t>(nullptr);
const intptr_t n7 = (intptr_t)nullptr;

decltype(nullptr) mynull = 0;
const int n8 = static_cast<const int>(mynull); // { dg-error "16:invalid 'static_cast' " }
const short int n9 = reinterpret_cast<short int>(mynull); // { dg-error "22:cast from .std::nullptr_t. to .short int. loses precision" }
const intptr_t n10 = reinterpret_cast<intptr_t>(mynull);
const intptr_t n11 = (intptr_t)mynull;
