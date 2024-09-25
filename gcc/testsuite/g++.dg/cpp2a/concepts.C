// { dg-do compile { target c++20 } }

// Change in grammar for the expression trailing `requires`.
template<typename T>
  requires true != false // { dg-error "must be enclosed" }
void f1(T)
{ }

template<typename T>
void f3(T) requires true != false // { dg-error "must be enclosed" }
{ }

template<typename T>
  requires requires {
    requires true == false; // OK: logical-or-expressions allowed here.
  }
void f3(T)
{ }

template<typename T>
concept bool C1 = true; // { dg-error "bool|variable concepts" }
template<typename T>
bool concept C2 = true; // { dg-error "concept definition syntax|variable concepts" }

template<typename T>
concept C3 = true; // OK
template<typename T>
concept C3 = true; // { dg-error "redefinition" }
template<typename T, typename U>
concept C3 = true; // { dg-error "different template parameters" }
template<int>
concept C3 = true; // { dg-error "different template parameters" }
int C3 = 0; // { dg-error "different kind of entity" }

int C4 = 0;
template<typename T>
concept C4 = true; // { dg-error "different kind of entity" }

// Concepts as expressions

template<typename T>
concept True = true;

template<typename T>
concept False = false;

static_assert(True<int>);
static_assert(False<int>); // { dg-error "static assertion failed" }

void f4(True auto);

template<True T> concept C5 = true; // { dg-error "cannot be constrained" }


template<typename T>
concept Recursive = Recursive<T>; // { dg-error "not declared|expected" }

