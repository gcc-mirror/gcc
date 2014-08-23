// PR c++/59349
// { dg-do compile { target c++14 } }

int foo () {
  [bar()]{};			// { dg-error "empty initializer" }
}
