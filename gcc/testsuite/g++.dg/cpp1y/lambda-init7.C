// PR c++/59349
// { dg-options "-std=c++1y" }

int foo () {
  [bar()]{};			// { dg-error "empty initializer" }
}
