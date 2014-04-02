// PR c++/59349
// { dg-do compile { target c++1y } }

int foo () {
  [bar()]{};			// { dg-error "empty initializer" }
}
