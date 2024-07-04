// PR c++/111660
// { dg-do compile { target c++20 } }

consteval int id (int i) { return i; }

void
g (int i)
{
  1 ? 1 : ((1 ? 1 : 1), id (i)); // { dg-error "call to consteval function|'i' is not a constant expression" }
  1 ? 1 : ((1 ? 1 : 1), id (i), 1); // { dg-error "call to consteval function|'i' is not a constant expression" }
  1 ? 1 : ((i ? 1 : 1), id (i), 1); // { dg-error "call to consteval function|'i' is not a constant expression" }
  1 ? 1 : ((1 ? i : 1), id (i), 1); // { dg-error "call to consteval function|'i' is not a constant expression" }
  1 ? 1 : ((1 ? 1 : i), id (i), 1); // { dg-error "call to consteval function|'i' is not a constant expression" }
  1 ? 1 : ((i ? -i : i), id (i), 1); // { dg-error "call to consteval function|'i' is not a constant expression" }
  1 ? 1 : ((1 ? 1 : id (i)), id (42), 1); // { dg-error "call to consteval function|'i' is not a constant expression" }
  1 ? 1 : ((1 ? 1 : id (42)), id (i)); // { dg-error "call to consteval function|'i' is not a constant expression" }
  1 ? 1 : ((1 ? 1 : id (42)), id (i), 1); // { dg-error "call to consteval function|'i' is not a constant expression" }
  id (i) ? 1 : ((1 ? 1 : 1), id (i)); // { dg-error "call to consteval function|'i' is not a constant expression" }
  1 ? 1 : ((1 ? 1 : id (i)), id (i)); // { dg-error "call to consteval function|'i' is not a constant expression" }
  1 ? id (i) : ((1 ? 1 : id (i)), id (i)); // { dg-error "call to consteval function|'i' is not a constant expression" }
  1 ? 1 : ((id (i) ? 1 : 1), id (i)); // { dg-error "call to consteval function|'i' is not a constant expression" }
}
